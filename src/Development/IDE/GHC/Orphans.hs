-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#include "ghc-api-version.h"

-- | Orphan instances for GHC.
--   Note that the 'NFData' instances may not be law abiding.
module Development.IDE.GHC.Orphans() where

import GHC
import GhcPlugins
import Development.IDE.GHC.Compat
import qualified StringBuffer as SB
import Control.DeepSeq
import Data.Hashable
import Development.IDE.GHC.Util


-- Orphan instances for types from the GHC API.
instance Show CoreModule where show = prettyPrint
instance NFData CoreModule where rnf = rwhnf
instance Show CgGuts where show = prettyPrint . cg_module
instance NFData CgGuts where rnf = rwhnf
instance Show ModDetails where show = const "<moddetails>"
instance NFData ModDetails where rnf = rwhnf
instance NFData SafeHaskellMode where rnf = rwhnf
instance Show Linkable where show = prettyPrint
instance NFData Linkable where rnf = rwhnf

#if MIN_GHC_API_VERSION(8,11,0)
instance Show UnitId where
    show = unitIdString

instance NFData UnitId where rnf = rwhnf . unitIdFS
#else
instance Show InstalledUnitId where
    show = installedUnitIdString

instance NFData InstalledUnitId where rnf = rwhnf . unitIdFS
#endif

instance NFData SB.StringBuffer where rnf = rwhnf

instance Show Module where
    show = moduleNameString . moduleName

instance Show (GenLocated SrcSpan ModuleName) where show = prettyPrint

instance (NFData l, NFData e) => NFData (GenLocated l e) where
    rnf (L l e) = rnf l `seq` rnf e

instance Show ModSummary where
    show = show . ms_mod

instance Show ParsedModule where
    show = show . pm_mod_summary

instance NFData ModSummary where
    rnf = rwhnf

#if !MIN_GHC_API_VERSION(8,10,0)
instance NFData FastString where
    rnf = rwhnf
#endif

instance NFData ParsedModule where
    rnf = rwhnf

#if MIN_GHC_API_VERSION(8,11,0)
instance Hashable UnitId where
  hashWithSalt salt = hashWithSalt salt . unitIdString
#else
instance Hashable InstalledUnitId where
  hashWithSalt salt = hashWithSalt salt . installedUnitIdString
#endif
instance Show HieFile where
    show = show . hie_module

instance NFData HieFile where
    rnf = rwhnf

deriving instance Eq SourceModified
deriving instance Show SourceModified
instance NFData SourceModified where
    rnf = rwhnf
