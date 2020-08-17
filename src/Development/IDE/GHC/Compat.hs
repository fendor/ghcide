-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS -Wno-dodgy-imports #-}
#include "ghc-api-version.h"

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    getHeaderImports,
    HieFileResult(..),
    HieFile(..),
    NameCacheUpdater(..),
    hieExportNames,
    mkHieFile,
    writeHieFile,
    readHieFile,
    supportsHieFiles,
    setHieDir,
    dontWriteHieFiles,
#if !MIN_GHC_API_VERSION(8,8,0)
    ml_hie_file,
#endif
    hPutStringBuffer,
    includePathsGlobal,
    includePathsQuote,
    addIncludePathsQuote,
    getModuleHash,
    getPackageName,
    Development.IDE.GHC.Compat.lookupModuleWithSuggestions,
    pattern Development.IDE.GHC.Compat.RealSrcLoc,
    pattern Development.IDE.GHC.Compat.RealSrcSpan,
    pattern Development.IDE.GHC.Compat.UnhelpfulSpan,
    pattern DerivD,
    pattern ForD,
    pattern InstD,
    pattern TyClD,
    pattern ValD,
    pattern SigD,
    pattern TypeSig,
    pattern ClassOpSig,
    pattern IEThingAll,
    pattern IEThingWith,
    pattern IEThingWithPs,
    pattern VarPatTc,
    pattern VarPatPs,
    pattern ConPat,
    pattern PatSynBind,
    GHC.ModLocation,
    Module.addBootSuffix,
    pattern ModLocation,
    getConArgs,

    HasSrcSpan,
    getLoc,

    upNameCache,
    Development.IDE.GHC.Compat.toUnitId,
    Development.IDE.GHC.Compat.isSse2Enabled,
    Development.IDE.GHC.Compat.isSseEnabled,
    Development.IDE.GHC.Compat.getUnitIncludePath,
    UnitInfo,
    showSDoc,
    showSDocUnsafe,
    pprPanic,
    InstalledUnitId,
    lookupUnitId',
    explicitUnits,
    Development.IDE.GHC.Compat.lookupUnitId,
    getUnitInfoMap,

    set_hsc_dflags,
    set_hsc_HPT,
    hsc_dflags,
    hsc_HPT,
    setNoCode,
    homeUnitId_,
    homeUnit_,
    toInstalledUnitId,
    hDuplicateTo',
    formatErrDoc,

    module GHC,
#if MIN_GHC_API_VERSION(8,6,0)

#if MIN_GHC_API_VERSION(8,8,0)
    module HieTypes,
    module HieUtils,
#else
    module Development.IDE.GHC.HieTypes,
    module Development.IDE.GHC.HieUtils,
#endif

#endif
    ) where

import StringBuffer
import DynFlags
import FieldLabel
import Fingerprint (Fingerprint)
import qualified Module
import Packages
import Data.IORef
import HscTypes
import NameCache

import qualified ErrUtils

import qualified GHC
import GHC hiding (
      ClassOpSig,
      DerivD,
      ForD,
      IEThingAll,
      IEThingWith,
      InstD,
      TyClD,
      ValD,
      SigD,
      TypeSig,
      VarPat,
      ModLocation,
      HasSrcSpan,
      PatSynBind,
      lookupName,
      RealSrcSpan,
      UnhelpfulSpan,
      getLoc

#if MIN_GHC_API_VERSION(8,11,0)
    , ConPat
    , RealSrcLoc
#else
    , ConPatOut
#endif
#if MIN_GHC_API_VERSION(8,6,0)
    , getConArgs
#endif
    )
import qualified HeaderInfo as Hdr
import Avail
import ErrUtils (ErrorMessages)
import FastString (FastString, fsLit)

#if MIN_GHC_API_VERSION(8,6,0)
import Development.IDE.GHC.HieAst (mkHieFile)
import Development.IDE.GHC.HieBin

#if MIN_GHC_API_VERSION(8,8,0)
import HieUtils
import HieTypes
#else
import Development.IDE.GHC.HieUtils
import Development.IDE.GHC.HieTypes
import System.FilePath ((-<.>))
#endif

#endif
#if MIN_GHC_API_VERSION(8,11,0)
import GHC.Driver.Ppr (showSDoc)
import GHC.Utils.Panic (pprPanic)
import GHC.Driver.Types (set_hsc_dflags, set_hsc_HPT)
import GHC.IO.Handle
import GHC.Unit.Home
import GHC.Types.SrcLoc (UnhelpfulSpanReason(..))
#else
import Outputable (showSDoc)
#endif

import Outputable (SDoc)

#if !MIN_GHC_API_VERSION(8,8,0)

#if MIN_GHC_API_VERSION(8,6,0)
import GhcPlugins (srcErrorMessages)
#else
import System.IO.Error
import IfaceEnv
import Binary
import Data.ByteString (ByteString)
import GhcPlugins (Hsc, srcErrorMessages)
import TcRnTypes
import MkIface
#endif

import Control.Exception (catch)
import System.IO
import Foreign.ForeignPtr

hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl (StringBuffer buf len cur)
    = withForeignPtr (plusForeignPtr buf cur) $ \ptr ->
             hPutBuf hdl ptr len

#endif

showSDocUnsafe :: SDoc -> String
showSDocUnsafe = showSDoc unsafeGlobalDynFlags

setNoCode :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 811
setNoCode d = d { GHC.backend = GHC.NoBackend }
#else
setNoCode d = d { GHC.hscTarget = GHC.HscNothing }
#endif

#if MIN_GHC_API_VERSION(8,6,0)
supportsHieFiles :: Bool
supportsHieFiles = True

hieExportNames :: HieFile -> [(SrcSpan, Name)]
hieExportNames = nameListFromAvails . hie_exports

#if !MIN_GHC_API_VERSION(8,8,0)
ml_hie_file :: GHC.ModLocation -> FilePath
ml_hie_file ml = ml_hi_file ml -<.> ".hie"
#endif

#endif

upNameCache :: IORef NameCache -> (NameCache -> (NameCache, c)) -> IO c
#if !MIN_GHC_API_VERSION(8,8,0)
upNameCache ref upd_fn
  = atomicModifyIORef' ref upd_fn
#else
upNameCache = updNameCache
#endif
#if !MIN_GHC_API_VERSION(8,6,0)
includePathsGlobal, includePathsQuote :: [String] -> [String]
includePathsGlobal = id
includePathsQuote = const []
#endif


addIncludePathsQuote :: FilePath -> DynFlags -> DynFlags
#if MIN_GHC_API_VERSION(8,6,0)
addIncludePathsQuote path x = x{includePaths = f $ includePaths x}
    where f i = i{includePathsQuote = path : includePathsQuote i}
#else
addIncludePathsQuote path x = x{includePaths = path : includePaths x}
#endif

pattern UnhelpfulSpan :: FastString -> SrcSpan
pattern UnhelpfulSpan x <-
#if MIN_GHC_API_VERSION(8,11,0)
    GHC.UnhelpfulSpan (toFsLit -> x)
    where UnhelpfulSpan x = GHC.UnhelpfulSpan (UnhelpfulOther x)

toFsLit :: UnhelpfulSpanReason -> FastString
toFsLit reason = case reason of
    UnhelpfulNoLocationInfo -> fsLit "<no location info>"
    UnhelpfulWiredIn -> fsLit "<wired into compiler>"
    UnhelpfulInteractive -> fsLit "<interactive>"
    UnhelpfulGenerated -> fsLit "<compiler-generated code>"
    UnhelpfulOther fs -> fs
#else
    GHC.UnhelpfulSpan x
#endif

pattern RealSrcLoc :: GHC.RealSrcLoc -> SrcLoc
pattern RealSrcLoc x <-
#if MIN_GHC_API_VERSION(8,11,0)
    GHC.RealSrcLoc x _
    where RealSrcLoc x = GHC.RealSrcLoc x Nothing
#else
    GHC.RealSrcLoc x
    where RealSrcLoc x = GHC.RealSrcLoc x
#endif


pattern RealSrcSpan :: GHC.RealSrcSpan -> SrcSpan
pattern RealSrcSpan x <-
#if MIN_GHC_API_VERSION(8,11,0)
    GHC.RealSrcSpan x _
    where RealSrcSpan x = GHC.RealSrcSpan x Nothing
#else
    GHC.RealSrcSpan x
    where RealSrcSpan x = GHC.RealSrcSpan x
#endif

pattern DerivD :: DerivDecl p -> HsDecl p
pattern DerivD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.DerivD _ x
#else
    GHC.DerivD x
#endif

pattern ForD :: ForeignDecl p -> HsDecl p
pattern ForD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ForD _ x
#else
    GHC.ForD x
#endif

pattern ValD :: HsBind p -> HsDecl p
pattern ValD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ValD _ x
#else
    GHC.ValD x
#endif

pattern InstD :: InstDecl p -> HsDecl p
pattern InstD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.InstD _ x
#else
    GHC.InstD x
#endif

pattern TyClD :: TyClDecl p -> HsDecl p
pattern TyClD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.TyClD _ x
#else
    GHC.TyClD x
#endif

pattern SigD :: Sig p -> HsDecl p
pattern SigD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.SigD _ x
#else
    GHC.SigD x
#endif

pattern TypeSig :: [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> Sig GhcPs
pattern TypeSig x y <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.TypeSig _ x y
#else
    GHC.TypeSig x y
#endif


pattern ClassOpSig :: Bool -> [Located (IdP GhcPs)] -> LHsSigType GhcPs -> Sig GhcPs
pattern ClassOpSig a b c <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ClassOpSig _ a b c
#else
    GHC.ClassOpSig a b c
#endif

pattern IEThingWithPs :: LIEWrappedName (IdP GhcPs) -> IEWildcard -> [LIEWrappedName (IdP GhcPs)] -> [Located (FieldLbl (IdP GhcPs))] -> IE GhcPs
pattern IEThingWithPs a b c d <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.IEThingWith _ a b c d
#else
    GHC.IEThingWith a b c d
#endif

pattern IEThingWith :: LIEWrappedName (IdP pass) -> [LIEWrappedName (IdP pass)] -> IE pass
pattern IEThingWith a c <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.IEThingWith _ a _ c _
#else
    GHC.IEThingWith a _ c _
#endif


pattern ModLocation :: Maybe FilePath -> FilePath -> FilePath -> GHC.ModLocation
pattern ModLocation a b c <-
#if MIN_GHC_API_VERSION(8,8,0)
    GHC.ModLocation a b c _ where ModLocation a b c = GHC.ModLocation a b c ""
#else
    GHC.ModLocation a b c where ModLocation a b c = GHC.ModLocation a b c
#endif

pattern IEThingAll :: LIEWrappedName (IdP pass) -> IE pass
pattern IEThingAll a <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.IEThingAll _ a
#else
    GHC.IEThingAll a
#endif

pattern ConPat :: Located (ConLikeP GhcTc) -> Pat GhcTc
pattern ConPat x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ConPat _ x _
#else
    GHC.ConPatOut x _ _ _ _ _ _
#endif

pattern VarPatTc :: Located (IdP GhcTc) -> Pat GhcTc
pattern VarPatTc x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.VarPat _ x
#else
    GHC.VarPat x
#endif

pattern VarPatPs :: Located (IdP GhcPs) -> Pat GhcPs
pattern VarPatPs x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.VarPat _ x
#else
    GHC.VarPat x
#endif

pattern PatSynBind :: GHC.PatSynBind p p -> HsBind p
pattern PatSynBind x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.PatSynBind _ x
#else
    GHC.PatSynBind x
#endif

setHieDir :: FilePath -> DynFlags -> DynFlags
setHieDir _f d =
#if MIN_GHC_API_VERSION(8,8,0)
    d { hieDir     = Just _f}
#else
    d
#endif

dontWriteHieFiles :: DynFlags -> DynFlags
dontWriteHieFiles d =
#if MIN_GHC_API_VERSION(8,8,0)
    gopt_unset d Opt_WriteHie
#else
    d
#endif

nameListFromAvails :: [AvailInfo] -> [(SrcSpan, Name)]
nameListFromAvails as =
  map (\n -> (nameSrcSpan n, n)) (concatMap availNames as)

#if !MIN_GHC_API_VERSION(8,6,0)
-- Reimplementations of functions for HIE files for GHC 8.6

mkHieFile :: ModSummary -> TcGblEnv -> RenamedSource -> ByteString -> Hsc HieFile
mkHieFile ms ts _ _ = return (HieFile (ms_mod ms) es)
  where
    es = nameListFromAvails (mkIfaceExports (tcg_exports ts))

ml_hie_file :: GHC.ModLocation -> FilePath
ml_hie_file ml = ml_hi_file ml ++ ".hie"

data HieFile = HieFile {hie_module :: Module, hie_exports :: [(SrcSpan, Name)]}

hieExportNames :: HieFile -> [(SrcSpan, Name)]
hieExportNames = hie_exports

instance Binary HieFile where
  put_ bh (HieFile m es) = do
    put_ bh m
    put_ bh es

  get bh = do
    mod <- get bh
    es <- get bh
    return (HieFile mod es)

data HieFileResult = HieFileResult { hie_file_result :: HieFile }

writeHieFile :: FilePath -> HieFile -> IO ()
readHieFile :: NameCacheUpdater -> FilePath -> IO HieFileResult
supportsHieFiles :: Bool

#if MIN_GHC_API_VERSION(8,4,0)

supportsHieFiles = False

writeHieFile _ _ = return ()

readHieFile _ fp = ioError $ mkIOError doesNotExistErrorType "" Nothing (Just fp)

#endif

#endif

getHeaderImports
  :: DynFlags
  -> StringBuffer
  -> FilePath
  -> FilePath
  -> IO
       ( Either
           ErrorMessages
           ( [(Maybe FastString, Located ModuleName)]
           , [(Maybe FastString, Located ModuleName)]
           , Located ModuleName
           )
       )
#if MIN_GHC_API_VERSION(8,8,0)
getHeaderImports = Hdr.getImports

#if MIN_GHC_API_VERSION(8,8,0)
class HasSrcSpan a where
    getLoc :: a -> SrcSpan
instance HasSrcSpan Name where
    getLoc = nameSrcSpan
instance HasSrcSpan (GenLocated SrcSpan a) where
    getLoc = GHC.getLoc
#else
type HasSrcSpan = GHC.HasSrcSpan
getLoc :: HasSrcSpan a => a -> SrcSpan
getLoc = GHC.getLoc
#endif

#else

class HasSrcSpan a where
    getLoc :: a -> SrcSpan
instance HasSrcSpan Name where
    getLoc = nameSrcSpan
instance HasSrcSpan (GenLocated SrcSpan a) where
    getLoc = GHC.getLoc

getHeaderImports a b c d =
    catch (Right <$> Hdr.getImports a b c d)
          (return . Left . srcErrorMessages)
#endif

getModuleHash :: ModIface -> Fingerprint
#if MIN_GHC_API_VERSION(8,10,0)
getModuleHash = mi_mod_hash . mi_final_exts
#else
getModuleHash = mi_mod_hash
#endif

getConArgs :: ConDecl GhcRn -> HsConDeclDetails GhcRn
#if MIN_GHC_API_VERSION(8,6,0)
getConArgs = GHC.getConArgs
#else
getConArgs = GHC.getConDetails
#endif

#if MIN_GHC_API_VERSION(8,11,0)
getPackageName :: DynFlags -> Module.UnitId -> Maybe PackageName
getPackageName dfs i = unitPackageName <$> Packages.lookupUnitId (unitState dfs) i
#else
getPackageName :: DynFlags -> Module.InstalledUnitId -> Maybe PackageName
getPackageName dfs i = packageName <$> lookupPackage dfs (Module.DefiniteUnitId (Module.DefUnitId i))
#endif

#if MIN_GHC_API_VERSION (8,11,0)
getUnitIncludePath :: DynFlags -> [Module.UnitId] -> IO [FilePath]
getUnitIncludePath dflags = do
    let sDocContext = initDefaultSDocContext dflags
    Packages.getUnitIncludePath sDocContext (unitState dflags) (mkHomeUnitFromFlags dflags)
#else
getUnitIncludePath :: DynFlags -> [Module.PreloadUnitId] -> IO [FilePath]
getUnitIncludePath dflags =
    Packages.getPackageIncludePath dflags
#endif

#if MIN_GHC_API_VERSION (8,11,0)
isSseEnabled :: DynFlags -> Bool
isSseEnabled = DynFlags.isSseEnabled . targetPlatform

isSse2Enabled :: DynFlags -> Bool
isSse2Enabled = DynFlags.isSse2Enabled . targetPlatform

getUnitInfoMap :: DynFlags -> UnitInfoMap
getUnitInfoMap = unitInfoMap . unitState

type InstalledUnitId = Module.UnitId

hDuplicateTo' :: Handle -> Handle -> IO ()
hDuplicateTo' = hDuplicateTo

formatErrDoc :: DynFlags -> ErrUtils.ErrDoc -> SDoc
formatErrDoc d = ErrUtils.formatErrDoc (initDefaultSDocContext d)

lookupModuleWithSuggestions :: DynFlags -> ModuleName -> Maybe FastString -> LookupResult
lookupModuleWithSuggestions d = Packages.lookupModuleWithSuggestions (unitState d)

toUnitId :: Unit -> Module.UnitId
toUnitId = Module.toUnitId

toInstalledUnitId :: InstalledUnitId -> Module.UnitId
toInstalledUnitId = id

homeUnit_ :: DynFlags -> Module.Unit
homeUnit_ = homeUnitAsUnit . mkHomeUnitFromFlags

lookupUnitId :: DynFlags -> Module.UnitId -> Maybe UnitInfo
lookupUnitId d u = Packages.lookupUnitId (unitState d) u

#else

formatErrDoc :: DynFlags -> ErrUtils.ErrDoc -> SDoc
formatErrDoc = ErrUtils.formatErrDoc

unitState :: DynFlags -> PackageState
unitState = GHC.pkgState

explicitUnits :: PackageState -> [UnitId]
explicitUnits = GHC.explicitPackages

lookupUnitId :: DynFlags -> UnitId -> Maybe PackageConfig
lookupUnitId = GHC.lookupPackage

lookupUnitId' :: PackageConfigMap -> UnitId -> Maybe PackageConfig
lookupUnitId' = GHC.lookupPackage' False

getUnitInfoMap :: DynFlags -> PackageConfigMap
getUnitInfoMap = GHC.getPackageConfigMap

set_hsc_dflags :: DynFlags -> HscEnv > HscEnv
set_hsc_dflags d h = h { hsc_dflags = d }

set_hsc_HPT :: HomePackageTable -> HscEnv > HscEnv
set_hsc_HPT d h = h { hsc_HPT = d }

type UnitInfo = PackageConfig


-- | A slightly modified version of 'hDuplicateTo' from GHC.
--   Importantly, it avoids the bug listed in https://gitlab.haskell.org/ghc/ghc/merge_requests/2318.
hDuplicateTo' :: Handle -> Handle -> IO ()
hDuplicateTo' h1@(FileHandle path m1) h2@(FileHandle _ m2)  = do
 withHandle__' "hDuplicateTo" h2 m2 $ \h2_ -> do
   -- The implementation in base has this call to hClose_help.
   -- _ <- hClose_help h2_
   -- hClose_help does two things:
   -- 1. It flushes the buffer, we replicate this here
   _ <- flushWriteBuffer h2_ `catch` \(_ :: IOException) -> pure ()
   -- 2. It closes the handle. This is redundant since dup2 takes care of that
   -- but even worse it is actively harmful! Once the handle has been closed
   -- another thread is free to reallocate it. This leads to dup2 failing with EBUSY
   -- if it happens just in the right moment.
   withHandle_' "hDuplicateTo" h1 m1 $ \h1_ -> do
     dupHandleTo path h1 Nothing h2_ h1_ (Just handleFinalizer)
hDuplicateTo' h1@(DuplexHandle path r1 w1) h2@(DuplexHandle _ r2 w2)  = do
 withHandle__' "hDuplicateTo" h2 w2  $ \w2_ -> do
   _ <- hClose_help w2_
   withHandle_' "hDuplicateTo" h1 w1 $ \w1_ -> do
     dupHandleTo path h1 Nothing w2_ w1_ (Just handleFinalizer)
 withHandle__' "hDuplicateTo" h2 r2  $ \r2_ -> do
   _ <- hClose_help r2_
   withHandle_' "hDuplicateTo" h1 r1 $ \r1_ -> do
     dupHandleTo path h1 (Just w1) r2_ r1_ Nothing
hDuplicateTo' h1 _ =
  ioe_dupHandlesNotCompatible h1

-- | This is copied unmodified from GHC since it is not exposed.
dupHandleTo :: FilePath
            -> Handle
            -> Maybe (MVar Handle__)
            -> Handle__
            -> Handle__
            -> Maybe HandleFinalizer
            -> IO Handle__
dupHandleTo filepath h other_side
            _hto_@Handle__{haDevice=devTo}
            h_@Handle__{haDevice=dev} mb_finalizer = do
  flushBuffer h_
  case cast devTo of
    Nothing   -> ioe_dupHandlesNotCompatible h
    Just dev' -> do
      _ <- IODevice.dup2 dev dev'
      FileHandle _ m <- dupHandle_ dev' filepath other_side h_ mb_finalizer
      takeMVar m

-- | This is copied unmodified from GHC since it is not exposed.
-- Note the beautiful inline comment!
dupHandle_ :: (IODevice dev, BufferedIO dev, Typeable dev) => dev
           -> FilePath
           -> Maybe (MVar Handle__)
           -> Handle__
           -> Maybe HandleFinalizer
           -> IO Handle
dupHandle_ new_dev filepath other_side _h_@Handle__{..} mb_finalizer = do
   -- XXX wrong!
  mb_codec <- if isJust haEncoder then fmap Just getLocaleEncoding else return Nothing
  mkHandle new_dev filepath haType True{-buffered-} mb_codec
      NewlineMode { inputNL = haInputNL, outputNL = haOutputNL }
      mb_finalizer other_side

-- | This is copied unmodified from GHC since it is not exposed.
ioe_dupHandlesNotCompatible :: Handle -> IO a
ioe_dupHandlesNotCompatible h =
   ioException (IOError (Just h) IllegalOperation "hDuplicateTo"
                "handles are incompatible" Nothing Nothing)

lookupModuleWithSuggestions :: DynFlags -> ModuleName -> Maybe FastString -> LookupResult
lookupModuleWithSuggestions = Packages.lookupModuleWithSuggestions

toUnitId :: Unit -> UnitId
toUnitId = moduleUnitId

homeUnitId_ :: DynFlags -> UnitId
homeUnitId_ = GHC.thisInstalledUnitId

homeUnit_ :: DynFlags -> Unit
homeUnit_ = GHC.thisPackage
#endif
