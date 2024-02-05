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
  pass wr       = MkWriterT $ \(ll, rr) => runWriterT wr <&> \((a, f), l, r) => (a, ll <+> f l, rr <+> r)
  listen wr     = MkWriterT $ \(ll, rr) => runWriterT wr <&> \(x, l, r) => ((x, l), ll <+> l, rr <+> r)

wrapFst : Functor m => WriterT r m a -> WriterT (l, r) m a
wrapFst $ MkWriterT fw = MkWriterT $ \(x, y) => map (x,) <$> fw y

unwrapFst : Functor m => Monoid l => WriterT (l, r) m a -> WriterT r m (a, l)
unwrapFst $ MkWriterT fw = MkWriterT $ \y => fw (neutral, y) <&> \(a, l, r) => ((a, l), r)

export
MonadWriter s (WriterT r m) => Monoid l => Monoid r => Monad m => MonadWriter s (WriterT (l, r) m) where
  writer (x, l) = wrapFst $ writer (x, l)
  tell          = wrapFst . tell
  pass wr       = MkWriterT $ \(x, y) => runWriterT (pass $ unwrapFst wr <&> \((a, f), l) => ((a, l), f)) <&> \((a, l), r) => (a, x <+> l, y <+> r)
  listen wr     = MkWriterT $ \(x, y) => runWriterT (listen $ unwrapFst wr) <&> \(((a, l), s), r) => ((a, s), x <+> l, y <+> r)

--- RWST ---

export
Monad m => Monoid wl => Monoid wr => MonadWriter wl (RWST r (wl, wr) s m) where
  writer (x, l) = writer (x, l, neutral)
  tell x        = tell (x, neutral)
  pass wr       = MkRWST $ \r, s, (ll, rr) => runRWST r s wr <&> \((a, f), s', x, y) => (a, s', ll <+> f x, rr <+> y)
  listen wr     = MkRWST $ \r, s, (ll, rr) => runRWST r s wr <&> \(a, s', x, y) => ((a, x), s', ll <+> x, rr <+> y)

wrapFst' : Functor m => RWST r wr s m a -> RWST r (wl, wr) s m a
wrapFst' $ MkRWST fw = MkRWST $ \r, s, (x, y) => map @{Compose} (x,) <$> fw r s y

unwrapFst' : Functor m => Monoid wl => RWST r (wl, wr) s m a -> RWST r wr s m (a, wl)
unwrapFst' $ MkRWST fw = MkRWST $ \r, s, y => fw r s (neutral, y) <&> \(a, s, wl, wr) => ((a, wl), s, wr)

export
MonadWriter v (RWST r wr s m) => Monoid wl => Monoid wr => Monad m => MonadWriter v (RWST r (wl, wr) s m) where
  writer (x, l) = wrapFst' $ writer (x, l)
  tell          = wrapFst' . tell
  pass  wr      = MkRWST $ \r, s, (x, y) => runRWST r s (pass $ unwrapFst' wr <&> \((a, f), l) => ((a, l), f)) <&>
                    \((a, wl), s, wr) => (a, s, x <+> wl, y <+> wr)
  listen wr     = MkRWST $ \r, s, (x, y) => runRWST r s (listen $ unwrapFst' wr) <&> \(((a, wl), v), s, wr) => ((a, v), s, x <+> wl, y <+> wr)
