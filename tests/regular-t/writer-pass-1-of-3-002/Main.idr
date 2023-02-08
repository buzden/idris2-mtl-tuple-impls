module Main

import Control.Monad.Writer.Tuple
import Control.Monad.Writer

action : MonadWriter (List Nat) m => Nat -> m String
action = map (const "done") . rec where
  rec : Nat -> m ()
  rec Z     = pure ()
  rec (S n) = tell [n] >> pass ((rec n <* tell [n+100]) <&> (, \xs => xs ++ [999] ++ map (+10) xs))

rw : (0 a, b, c : _) ->
     Show a => Show b => Show c => Show r => HasIO m =>
     WriterT (List a, List b, List c) m r -> m ()
rw _ _ _ wr = do
  (x, w) <- runWriterT wr
  putStrLn "result: \{show x}"
  putStrLn "journal: \{show w}"

rw' : (0 a : _) ->
      Show a => Show r => HasIO m =>
      WriterT (List a) m r -> m ()
rw' _ wr = do
  (x, w) <- runWriterT wr
  putStrLn "result: \{show x}"
  putStrLn "journal: \{show w}"

main : IO ()
main = do
  rw' Nat $ action 4
  putStrLn "----"
  putStrLn "----"
  rw Nat    String Double $ action 4
  putStrLn "----"
  rw String Nat    Double $ action 4
  putStrLn "----"
  rw Double String Nat    $ action 4
