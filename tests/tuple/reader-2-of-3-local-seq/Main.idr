module Main

import Control.Monad.Reader.Tuple
import Control.Monad.Reader

action : Monad m => MonadReader Nat m => MonadReader Double m => m String
action = do
  pre1 <- local S $ ("pre nat: " ++) . show . S <$> ask
  pre2 <- local S $ ("pre db: " ++) . show . sqrt <$> ask
  mid1 <- local (/2.0) . local S $ ("mid nat: " ++) . show . S <$> ask
  mid2 <- local (/2.0) . local S $ ("mid db: " ++) . show . (/2.0) <$> ask
  pst1 <- ("post nat: " ++) . show . S <$> ask
  pst2 <- ("post db:" ++) . show . sqrt <$> ask
  pure "\{pre1}, \{pre2}, \{mid1}, \{mid2}, \{pst1}, \{pst2}"

main : IO ()
main = do
  putStrLn =<< runReaderT {m=IO} (the Nat 10   , the Double 16, "three"   ) action
  putStrLn =<< runReaderT {m=IO} (the Double 16, the Nat 10   , "three"   ) action
  putStrLn =<< runReaderT {m=IO} ("one"        , the Double 16, the Nat 10) action
  putStrLn =<< runReaderT {m=IO} (the Double 16, "two"        , the Nat 10) action
