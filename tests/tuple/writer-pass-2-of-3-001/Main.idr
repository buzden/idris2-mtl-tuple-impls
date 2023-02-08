module Main

import Control.Monad.Writer.Tuple
import Control.Monad.Writer

action : Monad m => MonadWriter (List Nat) m => MonadWriter (List String) m => Nat -> m String
action = map (const "done") . rec where
  rec : Nat -> m ()
  rec Z     = pure ()
  rec (S n) = tell [show n] >> tell [n] >> tell ["."] >> do
    x <- pass {w=List Nat} $ rec n <&> (, \xs => xs ++ map (+10) xs)
    tell [show n ++ "'"]
    pure x

rw : (0 a, b, c : _) ->
     Show a => Show b => Show c => Show r => HasIO m =>
     WriterT (List a, List b, List c) m r -> m ()
rw _ _ _ wr = do
  (x, w) <- runWriterT wr
  putStrLn "result: \{show x}"
  putStrLn "journal: \{show w}"

main : IO ()
main = do
  rw Nat    String Double $ action 4
  putStrLn "----"
  rw String Nat    Double $ action 4
  putStrLn "----"
  rw Double String Nat    $ action 4
