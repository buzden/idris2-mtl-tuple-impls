module Main

import Control.Monad.Reader.Tuple
import Control.Monad.Reader

action : MonadReader Nat m => m String
action = do
  x <- local S $ ("pre: " ++) . show <$> ask
  y <- ("post: " ++) . show <$> ask
  pure "\{x}, \{y}"

main : IO ()
main = do
  putStrLn =<< runReaderT {m=IO} (the Nat 10, "two"     , "three"   ) action
  putStrLn =<< runReaderT {m=IO} ("one"     , the Nat 10, "three"   ) action
  putStrLn =<< runReaderT {m=IO} ("one"     , "two"     , the Nat 10) action
