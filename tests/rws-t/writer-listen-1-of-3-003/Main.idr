module Main

import Control.Monad.Writer.Tuple
import Control.Monad.RWS

action : Monad m => MonadWriter (List Nat) m => HasIO m => Nat -> m String
action = map (const "done") . rec where
  rec : Nat -> m ()
  rec Z     = pure ()
  rec (S n) = tell [n] >> do
    putStrLn "rec \{show $ S n} : before recursive call and listening"
    (x, log1) <- listen $ rec n
    (x, log2) <- listen $ rec n
    putStrLn "rec \{show $ S n} : current log1: \{show log1}"
    putStrLn "rec \{show $ S n} : current log2: \{show log2}"
    pure x

rw : (0 a, b, c : _) ->
     Show a => Show b => Show c => Show r => HasIO m =>
     RWST () (List a, List b, List c) () m r -> m ()
rw _ _ _ wr = do
  (x, (), w) <- runRWST () () wr
  putStrLn "result: \{show x}"
  putStrLn "journal: \{show w}"

main : IO ()
main = do
  rw Nat    String Double $ action 4
  putStrLn "----"
  rw String Nat    Double $ action 4
  putStrLn "----"
  rw Double String Nat    $ action 4
