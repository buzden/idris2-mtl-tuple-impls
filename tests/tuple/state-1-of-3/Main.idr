module Main

import Control.Monad.State.Tuple
import Control.Monad.State

action : MonadState Nat m => Nat -> m String
action = map (("res: " ++) . show) . rec where
  rec : Nat -> m Nat
  rec Z     = get
  rec (S n) = modify (* S n) >> rec n

main : IO ()
main = do
  putStrLn . show =<< runStateT {m=IO} (the Nat 10, "two"     , "three"   ) (action 4)
  putStrLn . show =<< runStateT {m=IO} ("one"     , the Nat 10, "three"   ) (action 4)
  putStrLn . show =<< runStateT {m=IO} ("one"     , "two"     , the Nat 10) (action 4)
