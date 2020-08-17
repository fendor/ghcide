-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | General utility functions, mostly focused around GHC operations.
module Development.IDE.GHC.Util(
    -- * HcsEnv and environment
    HscEnvEq,
    hscEnv, newHscEnvEq,
    modifyDynFlags,
    evalGhcEnv,
    runGhcEnv,
    deps,
    -- * GHC wrappers
    prettyPrint,
    printRdrName,
    printName,
    ParseResult(..), runParser,
    lookupPackageConfig,
    textToStringBuffer,
    bytestringToStringBuffer,
    stringBufferToByteString,
    moduleImportPath,
    cgGutsToCoreModule,
    fingerprintToBS,
    fingerprintFromStringBuffer,
    -- * General utilities
    readFileUtf8,
    GHC.hDuplicateTo',
    setHieDir,
    dontWriteHieFiles,
    ) where

import Control.Concurrent
import Data.List.Extra
import Data.ByteString.Internal (ByteString(..))
import Data.Maybe
import Data.Typeable
import qualified Data.ByteString.Internal as BS
import Fingerprint
import GhcMonad
import Control.Exception
import Data.IORef
import FileCleanup
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.IO.BufferedIO (BufferedIO)
import GHC.IO.Device as IODevice
import GHC.IO.Encoding
import GHC.IO.Exception
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import Data.Unique
import Development.Shake.Classes
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString          as BS
import Lexer
import StringBuffer
import System.FilePath
import HscTypes (cg_binds, md_types, cg_module, ModDetails
                , CgGuts, ic_dflags, hsc_IC, HscEnv)
import Outputable (ppr, Outputable)
import SrcLoc (mkRealSrcLoc)
import FastString (mkFastString)
import DynFlags (emptyFilesToClean, unsafeGlobalDynFlags)
import Module (moduleNameSlashes, UnitId)
import OccName (parenSymOcc)
import RdrName (nameRdrName, rdrNameOcc)

import Development.IDE.GHC.Compat as GHC
import Development.IDE.Types.Location


----------------------------------------------------------------------
-- GHC setup

-- | Used to modify dyn flags in preference to calling 'setSessionDynFlags',
--   since that function also reloads packages (which is very slow).
modifyDynFlags :: GhcMonad m => (DynFlags -> DynFlags) -> m ()
modifyDynFlags f = do
  newFlags <- f <$> getSessionDynFlags
  -- We do not use setSessionDynFlags here since we handle package
  -- initialization separately.
  modifySession $ \h ->
    set_hsc_dflags newFlags h { hsc_IC = (hsc_IC h) {ic_dflags = newFlags} }

-- | Given a 'UnitId' try and find the associated 'PackageConfig' in the environment.
lookupPackageConfig :: UnitId -> HscEnv -> Maybe GHC.UnitInfo
lookupPackageConfig unitId env =
    GHC.lookupUnitId' pkgConfigMap unitId
    where
        pkgConfigMap =
            -- For some weird reason, the GHC API does not provide a way to get the PackageConfigMap
            -- from PackageState so we have to wrap it in DynFlags first.
            GHC.getUnitInfoMap $ hsc_dflags env


-- | Convert from the @text@ package to the @GHC@ 'StringBuffer'.
--   Currently implemented somewhat inefficiently (if it ever comes up in a profile).
textToStringBuffer :: T.Text -> StringBuffer
textToStringBuffer = stringToStringBuffer . T.unpack

runParser :: DynFlags -> String -> P a -> ParseResult a
runParser flags str parser = unP parser parseState
    where
      filename = "<interactive>"
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
      parseState = mkPState flags buffer location

stringBufferToByteString :: StringBuffer -> ByteString
stringBufferToByteString StringBuffer{..} = PS buf cur len

bytestringToStringBuffer :: ByteString -> StringBuffer
bytestringToStringBuffer (PS buf cur len) = StringBuffer{..}

-- | Pretty print a GHC value using 'unsafeGlobalDynFlags '.
prettyPrint :: Outputable a => a -> String
prettyPrint = showSDoc unsafeGlobalDynFlags . ppr

-- | Pretty print a 'RdrName' wrapping operators in parens
printRdrName :: RdrName -> String
printRdrName name = showSDoc unsafeGlobalDynFlags $ parenSymOcc rn (ppr rn)
  where
    rn = rdrNameOcc name

-- | Pretty print a 'Name' wrapping operators in parens
printName :: Name -> String
printName = printRdrName . nameRdrName

-- | Run a 'Ghc' monad value using an existing 'HscEnv'. Sets up and tears down all the required
--   pieces, but designed to be more efficient than a standard 'runGhc'.
evalGhcEnv :: HscEnv -> Ghc b -> IO b
evalGhcEnv env act = snd <$> runGhcEnv env act

-- | Run a 'Ghc' monad value using an existing 'HscEnv'. Sets up and tears down all the required
--   pieces, but designed to be more efficient than a standard 'runGhc'.
runGhcEnv :: HscEnv -> Ghc a -> IO (HscEnv, a)
runGhcEnv env act = do
    filesToClean <- newIORef emptyFilesToClean
    dirsToClean <- newIORef mempty
    let dflags = (hsc_dflags env){filesToClean=filesToClean, dirsToClean=dirsToClean, useUnicode=True}
    ref <- newIORef $ set_hsc_dflags dflags env
    res <- unGhc act (Session ref) `finally` do
        cleanTempFiles dflags
        cleanTempDirs dflags
    (,res) <$> readIORef ref

-- | Given a module location, and its parse tree, figure out what is the include directory implied by it.
--   For example, given the file @\/usr\/\Test\/Foo\/Bar.hs@ with the module name @Foo.Bar@ the directory
--   @\/usr\/Test@ should be on the include path to find sibling modules.
moduleImportPath :: NormalizedFilePath -> GHC.ModuleName -> Maybe FilePath
-- The call to takeDirectory is required since DAML does not require that
-- the file name matches the module name in the last component.
-- Once that has changed we can get rid of this.
moduleImportPath (takeDirectory . fromNormalizedFilePath -> pathDir) mn
    -- This happens for single-component modules since takeDirectory "A" == "."
    | modDir == "." = Just pathDir
    | otherwise = dropTrailingPathSeparator <$> stripSuffix modDir pathDir
  where
    -- A for module A.B
    modDir =
        takeDirectory $
        fromNormalizedFilePath $ toNormalizedFilePath' $
        moduleNameSlashes mn

-- | An 'HscEnv' with equality. Two values are considered equal
--   if they are created with the same call to 'newHscEnvEq'.
data HscEnvEq
    = HscEnvEq !Unique !HscEnv
               [(InstalledUnitId, DynFlags)] -- In memory components for this HscEnv
               -- This is only used at the moment for the import dirs in
               -- the DynFlags

-- | Unwrap an 'HsEnvEq'.
hscEnv :: HscEnvEq -> HscEnv
hscEnv = either error id . hscEnv'

hscEnv' :: HscEnvEq -> Either String HscEnv
hscEnv' (HscEnvEq _ x _) = Right x
deps :: HscEnvEq -> [(InstalledUnitId, DynFlags)]
deps (HscEnvEq _ _ u) = u

-- | Wrap an 'HscEnv' into an 'HscEnvEq'.
newHscEnvEq :: HscEnv -> [(InstalledUnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEq e uids = do u <- newUnique; return $ HscEnvEq u e uids

instance Show HscEnvEq where
  show (HscEnvEq a _ _) = "HscEnvEq " ++ show (hashUnique a)

instance Eq HscEnvEq where
  HscEnvEq a _ _ == HscEnvEq b _ _ = a == b

instance NFData HscEnvEq where
  rnf (HscEnvEq a b c) = rnf (hashUnique a) `seq` b `seq` c `seq` ()

instance Hashable HscEnvEq where
  hashWithSalt s (HscEnvEq a _b _c) = hashWithSalt s a

-- Fake instance needed to persuade Shake to accept this type as a key.
-- No harm done as ghcide never persists these keys currently
instance Binary HscEnvEq where
  put _ = error "not really"
  get = error "not really"

-- | Read a UTF8 file, with lenient decoding, so it will never raise a decoding error.
readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 f = T.decodeUtf8With T.lenientDecode <$> BS.readFile f

-- | Convert from a 'CgGuts' to a 'CoreModule'.
cgGutsToCoreModule :: SafeHaskellMode -> CgGuts -> ModDetails -> CoreModule
cgGutsToCoreModule safeMode guts modDetails = CoreModule
    (cg_module guts)
    (md_types modDetails)
    (cg_binds guts)
    safeMode

-- | Convert a 'Fingerprint' to a 'ByteString' by copying the byte across.
--   Will produce an 8 byte unreadable ByteString.
fingerprintToBS :: Fingerprint -> BS.ByteString
fingerprintToBS (Fingerprint a b) = BS.unsafeCreate 8 $ \ptr -> do
    ptr <- pure $ castPtr ptr
    pokeElemOff ptr 0 a
    pokeElemOff ptr 1 b

-- | Take the 'Fingerprint' of a 'StringBuffer'.
fingerprintFromStringBuffer :: StringBuffer -> IO Fingerprint
fingerprintFromStringBuffer (StringBuffer buf len cur) =
    withForeignPtr buf $ \ptr -> fingerprintData (ptr `plusPtr` cur) len
