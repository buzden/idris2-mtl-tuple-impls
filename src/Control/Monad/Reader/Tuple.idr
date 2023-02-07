module Control.Monad.Reader.Tuple

import Control.Monad.Reader.Reader
import public Control.Monad.Reader.Interface
import Control.Monad.RWS.CPS

%default total

export
Monad m => MonadReader l (ReaderT (l, r) m) where
  ask   = Builtin.fst <$> ask
  local = local . mapFst

export
MonadReader v (ReaderT r m) => Monad m => MonadReader v (ReaderT (l, r) m) where
  ask   @{sub}       = MkReaderT $ runReaderT' (ask @{sub}) . snd
  local @{sub} f act = MkReaderT $ \(x, y) => runReaderT y $ local @{sub} f $ MkReaderT $ runReaderT' act . (x,)

export
Monad m => MonadReader rl (RWST (rl, rr) w s m) where
  ask   = Builtin.fst <$> ask
  local = local . mapFst

export
MonadReader v (RWST rr w s m) => Monad m => MonadReader v (RWST (rl, rr) w s m) where
  ask   @{sub}       = MkRWST $ unRWST (ask @{sub}) . snd
  local @{sub} f act = MkRWST $ \(x, y) => flip unRWST y $ local @{sub} f $ MkRWST $ unRWST act . (x,)
