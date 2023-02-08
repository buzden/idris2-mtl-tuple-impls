module Main

import Control.Monad.RWS
import Control.Monad.State.Tuple

action : Monad m => MonadState Nat m => MonadState String m => Nat -> m String
action = map ("res: " ++) . rec where
  rec : Nat -> m String
  rec Z     = pure "nat: \{show $ S !get}, str: \{the String !get}"
  rec (S n) = modify (* S n) >> modify (++ ".") >> rec n

main : IO ()
main = do
  putStrLn . show =<< runRWST {m=IO} {w=()} () (the Nat 10    , "two"         , the Double 3.0) (action 4)
  putStrLn . show =<< runRWST {m=IO} {w=()} () (the Double 1.0, the Nat 10    , "three"       ) (action 4)
  putStrLn . show =<< runRWST {m=IO} {w=()} () ("one"         , the Double 2.0, the Nat 10    ) (action 4)
