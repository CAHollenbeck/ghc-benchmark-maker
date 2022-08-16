import Test.QuickCheck
import Debug.Trace

main = do
  trace "TRACE" (sample dice)
  putStrLn("\n")
  sample dice2
  putStrLn("\n")
  sample dice3
  where dice  = choose (1, 6) :: Gen Int 
        dice2 = choose (1, 6) :: Gen Int
        dice3 = choose (1, 6) :: Gen Int
