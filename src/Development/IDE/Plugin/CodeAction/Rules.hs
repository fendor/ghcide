module Development.IDE.Plugin.CodeAction.Rules
  ( rulePackageExports
  )
where

import           Data.HashMap.Strict            ( fromListWith )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Traversable               ( forM )
import           Development.IDE.Core.Rules
import           Development.IDE.GHC.Util
import           Development.IDE.GHC.Compat as Compat
import           Development.IDE.Plugin.CodeAction.RuleTypes
import           Development.Shake
import           HscTypes                       ( IfaceExport
                                                , hsc_dflags
                                                , mi_exports
                                                )
import           LoadIface
import           Maybes
import           Module                         ( Module(..)
                                                , ModuleName
                                                , moduleNameString
                                                )
import           TcRnMonad                      ( WhereFrom(ImportByUser)
                                                , initIfaceLoad
                                                )
import GHC.Unit.Info
import GHC.Unit.Types

rulePackageExports :: Rules ()
rulePackageExports = defineNoFile $ \(PackageExports session) -> do
  let env     = hscEnv session
      pkgst   = Compat.unitState (hsc_dflags env)
      depends = Compat.explicitUnits pkgst
      targets =
        [ (d, pkg, mn)
        | d        <- depends
        , Just pkg <- [lookupPackageConfig (Compat.toUnitId d) env]
        , (mn, _)  <- unitExposedModules pkg
        ]

  results <- forM targets $ \(unit, pkg, mn) -> do
    modIface <- liftIO $ initIfaceLoad env $ loadInterface
      ""
      (mkModule unit mn)
      (ImportByUser NotBoot)
    case modIface of
      Maybes.Failed    _err -> return mempty
      Maybes.Succeeded mi   -> do
        let avails = mi_exports mi
        return $ concatMap (unpackAvail mn) avails
  return $ fromListWith (++) $ concat results

unpackAvail :: ModuleName -> IfaceExport -> [(Text, [(IdentInfo, Text)])]
unpackAvail mod =
  map (\id@IdentInfo {..} -> (name, [(id, pack $ moduleNameString mod)]))
    . mkIdentInfos
