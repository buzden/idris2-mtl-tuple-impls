module Main

import Control.Monad.Reader.Tuple
import Control.Monad.RWS

action : MonadReader Nat m => m String
action = local S $ ("res: " ++) . show <$> ask

main : IO ()
main = do
  putStrLn . show =<< runRWST {m=IO} {w=()} (the Nat 10, "two"     , "three"   ) () action
  putStrLn . show =<< runRWST {m=IO} {w=()} ("one"     , the Nat 10, "three"   ) () action
  putStrLn . show =<< runRWST {m=IO} {w=()} ("one"     , "two"     , the Nat 10) () action
