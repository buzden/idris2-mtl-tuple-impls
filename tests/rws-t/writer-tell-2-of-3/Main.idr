module Main

import Control.Monad.Writer.Tuple
import Control.Monad.RWS

action : Monad m => MonadWriter (List Nat) m => MonadWriter (List String) m => Nat -> m String
action = map (const "done") . rec where
  rec : Nat -> m ()
  rec Z     = pure ()
  rec (S n) = tell [show n] >> tell [n] >> tell ["."] >> rec n

rw : (0 a, b, c : _) ->
     Show a => Show b => Show c => Show r => HasIO m =>
     RWST () (List a, List b, List c) () m r -> m ()
rw _ _ _ wr = do
  (x, (), w) <- runRWST () () wr
  putStrLn "result: \{show x}, journal: \{show w}"

main : IO ()
main = do
  rw Nat    String Double $ action 4
  rw String Nat    Double $ action 4
  rw Double String Nat    $ action 4
