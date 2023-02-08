module Main

import Control.Monad.RWS
import Control.Monad.State.Tuple

action : MonadState Nat m => Nat -> m String
action = map (("res: " ++) . show) . rec where
  rec : Nat -> m Nat
  rec Z     = get
  rec (S n) = modify (* S n) >> rec n

main : IO ()
main = do
  putStrLn . show =<< runRWST {m=IO} {w=()} () (the Nat 10, "two"     , "three"   ) (action 4)
  putStrLn . show =<< runRWST {m=IO} {w=()} () ("one"     , the Nat 10, "three"   ) (action 4)
  putStrLn . show =<< runRWST {m=IO} {w=()} () ("one"     , "two"     , the Nat 10) (action 4)
