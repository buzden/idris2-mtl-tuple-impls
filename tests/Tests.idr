module Tests

import Test.Golden.RunnerHelper

main : IO ()
main = goldenRunner
  [ "Documentation" `atDir` "docs"
  , "Regular transformers (ReaderT, WriterT, StateT)" `atDir` "regular-t"
  , "RWST transformer" `atDir` "rws-t"
  ]
