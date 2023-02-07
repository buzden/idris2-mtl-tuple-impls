module Control.Monad.Writer.Tuple

import Control.Monad.Writer.CPS
import public Control.Monad.Writer.Interface
import Control.Monad.RWS.CPS

%default total

--- WriterT ---

export
Monad m => Monoid l => Monoid r => MonadWriter l (WriterT (l, r) m) where
  writer (x, l) = writer (x, l, neutral)
  tell x        = tell (x, neutral)
  pass          = MkWriterT . map (\((a, f), l, r) => (a, f l, r)) .: unWriterT
  listen        = MkWriterT . map (\(x, l, r) => ((x, l), l, r)) .: unWriterT

wrapFst : Functor m => WriterT r m a -> WriterT (l, r) m a
wrapFst $ MkWriterT fw = MkWriterT $ \(x, y) => map (x,) <$> fw y

unwrapFst : Functor m => l -> WriterT (l, r) m a -> WriterT r m a
unwrapFst x $ MkWriterT fw = MkWriterT $ \y => map snd <$> fw (x, y)

export
MonadWriter s (WriterT r m) => Monad m => MonadWriter s (WriterT (l, r) m) where
  writer (x, l) = wrapFst $ writer (x, l)
  tell          = wrapFst . tell
  pass  wr      = MkWriterT $ \(x, y) => map @{Compose} (x,) $ flip unWriterT y $ pass   $ unwrapFst x wr
  listen wr     = MkWriterT $ \(x, y) => map @{Compose} (x,) $ flip unWriterT y $ listen $ unwrapFst x wr

--- RWST ---

export
Monad m => Monoid wl => Monoid wr => MonadWriter wl (RWST r (wl, wr) s m) where
  writer (x, l) = writer (x, l, neutral)
  tell x        = tell (x, neutral)
  pass wr       = MkRWST $ (\x, y => x y <&> \((a, f), s, wl, wr) => (a, s, f wl, wr)) .: unRWST wr
  listen wr     = MkRWST $ (\x, y => x y <&> \(a, s, wl, wr) => ((a, wl), s, wl, wr)) .: unRWST wr

wrapFst' : Functor m => RWST r wr s m a -> RWST r (wl, wr) s m a
wrapFst' $ MkRWST fw = MkRWST $ \r, s, (x, y) => map @{Compose} (x,) <$> fw r s y

unwrapFst' : Functor m => wl -> RWST r (wl, wr) s m a -> RWST r wr s m a
unwrapFst' x $ MkRWST fw = MkRWST $ \r, s, y => map @{Compose} snd <$> fw r s (x, y)

export
MonadWriter v (RWST r wr s m) => Monad m => MonadWriter v (RWST r (wl, wr) s m) where
  writer (x, l) = wrapFst' $ writer (x, l)
  tell          = wrapFst' . tell
  pass  wr      = MkRWST $ \r, s, (x, y) => map @{Compose @{Compose}} (x,) $ unRWST (pass   $ unwrapFst' x wr) r s y
  listen wr     = MkRWST $ \r, s, (x, y) => map @{Compose @{Compose}} (x,) $ unRWST (listen $ unwrapFst' x wr) r s y
