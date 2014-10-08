module PreludeTests where

import Control.Monad.Eff
import Data.Eq
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.LCG

testConst :: Number -> Number -> Number -> Boolean
testConst a b c = const a b == const a c

mkMessage :: (Number -> Number) -> String
mkMessage f = "Test failed for function (" 
  ++ show (f 0) ++ ", " 
  ++ show (f 1) ++ ", " 
  ++ show (f 2) ++ ")"

main = do
  trace "Sample [Number]"
  showSample (chooseInt 0 10)

  trace "testConst:"
  quickCheck testConst

  trace "id is a left unit for <<<"
  quickCheck $ \f a -> ((id <<< f) (a :: Number) == (f a) :: Number) <?> mkMessage f

  trace "Precedence of && and ||:"
  quickCheck $ \a b c -> ((a :: Boolean && b) || c) == ((a || c) && (b || c))
  
  trace "Test Eq instance for Ref:"
  quickCheck $ \a -> (Ref a :: Ref Number) == Ref a
  quickCheck $ \a -> not $ (Ref a :: Ref Number /= Ref a)

