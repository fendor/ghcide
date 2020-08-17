{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"
module Development.IDE.GHC.WithDynFlags
( WithDynFlags
, evalWithDynFlags
) where

import Control.Monad.Trans.Reader (ask, ReaderT(..))
import GHC (DynFlags)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch
import Exception (ExceptionMonad(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import GhcPlugins (HasDynFlags(..))

-- | A monad transformer implementing the 'HasDynFlags' effect
newtype WithDynFlags m a = WithDynFlags {withDynFlags :: ReaderT DynFlags m a}
  deriving
    (Applicative, Functor, Monad, MonadIO, MonadTrans
#if MIN_GHC_API_VERSION(8,11,0)
    , MonadCatch, MonadMask, MonadThrow
#endif
    )

evalWithDynFlags :: DynFlags -> WithDynFlags m a -> m a
evalWithDynFlags dflags = flip runReaderT dflags . withDynFlags

instance Monad m => HasDynFlags (WithDynFlags m) where
    getDynFlags = WithDynFlags ask

#if !MIN_GHC_API_VERSION(8,11,0)
instance ExceptionMonad m => ExceptionMonad (WithDynFlags m) where
    gmask f = WithDynFlags $ ReaderT $ \env ->
        gmask $ \restore ->
            let restore' = lift . restore . flip runReaderT env . withDynFlags
            in runReaderT (withDynFlags $ f restore') env

    gcatch (WithDynFlags act) handle = WithDynFlags $ ReaderT $ \env ->
        gcatch (runReaderT act env) (flip runReaderT env . withDynFlags . handle)
#endif
