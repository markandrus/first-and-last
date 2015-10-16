import Test.DocTest (doctest)

main :: IO ()
main = doctest
  [ "-XDataKinds"
  , "src/Data/Monoid/First.hs"
  , "src/Data/Monoid/Last.hs"
  ]
