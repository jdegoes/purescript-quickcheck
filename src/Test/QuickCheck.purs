module Test.QuickCheck 
  ( (<?>)
  , AlphaNumString(..)
  , Arbitrary
  , CoArbitrary
  , Negative(..)
  , NonZero(..)
  , Positive(..)
  , QC(..)
  , quickCheck
  , quickCheck'
  , quickCheckPure
  , Result(..)
  , smallCheck
  , smallCheckPure
  , Testable
  ) where

import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Trampoline
import Control.Bind
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception
import Data.Array
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Traversable
import Math

import qualified Data.String as S

import Test.QuickCheck.LCG

class Arbitrary t where
  arbitrary :: Gen t

class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r

class Testable prop where
  test :: prop -> Gen Result

newtype AlphaNumString = AlphaNumString String

newtype Positive = Positive Number

newtype Negative = Negative Number

newtype NonZero  = NonZero Number

type QC a = forall eff. Eff (trace :: Trace, random :: Random, err :: Exception | eff) a

data Result = Success | Failed String

(<?>) :: Boolean -> String -> Result
(<?>) true  = const Success
(<?>) false = Failed

quickCheckPure :: forall prop. (Testable prop) => Number -> Number -> prop -> [Result]
quickCheckPure n s prop = runTrampoline $ sample' n (defState s) (test prop)

quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit
quickCheck' n prop = check (quickCheckPure n) prop

quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
quickCheck prop = quickCheck' 100 prop

smallCheckPure :: forall prop. (Testable prop) => Number -> prop -> [Result]
smallCheckPure s prop = runTrampoline $ collectAll (defState s) (test prop)

smallCheck :: forall prop. (Testable prop) => prop -> QC Unit
smallCheck prop = check smallCheckPure prop

defState :: Number -> GenState
defState s = (GenState {seed: s, size: 10})

check :: forall prop. (Testable prop) => (Number -> prop -> [Result]) -> prop -> QC Unit
check f prop = do
  seed <- random
  let results   = f seed prop
  let successes = countSuccesses results
  trace $ show successes ++ "/" ++ show (length results) ++ " test(s) passed."
  throwOnFirstFailure 1 results

throwOnFirstFailure :: Number -> [Result] -> QC Unit
throwOnFirstFailure _ []                  = return unit
throwOnFirstFailure n (Failed msg : _)    = throwException $ error $ "Test " ++ show n ++ " failed: \n" ++ msg
throwOnFirstFailure n (_          : rest) = throwOnFirstFailure (n + 1) rest

countSuccesses :: [Result] -> Number
countSuccesses = countSuccesses' 0 
  where countSuccesses' acc []               = acc
        countSuccesses' acc (Success : rest) = countSuccesses' (acc + 1) rest
        countSuccesses' acc (_       : rest) = countSuccesses' acc rest

foreign import maxNumber "var maxNumber = Number.MAX_VALUE;" :: Number
foreign import minNumber "var minNumber = Number.MIN_VALUE;" :: Number

instance showResult :: Show Result where
  show Success      = "Success"
  show (Failed msg) = "Failed " ++ msg

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform 

instance coarbNumber :: CoArbitrary Number where
  coarbitrary = perturbGen  

instance arbPositive :: Arbitrary Positive where
  arbitrary = Positive <$> ((*) maxNumber) <$> uniform

instance coarbPositive :: CoArbitrary Positive where
  coarbitrary (Positive n) = coarbitrary n

instance arbNegative :: Arbitrary Negative where
  arbitrary = Negative <$> ((*) minNumber) <$> uniform

instance coarbNegative :: CoArbitrary Negative where
  coarbitrary (Negative n) = coarbitrary n

instance arbNonZero :: Arbitrary NonZero where
  arbitrary = do n <- arbitrary
                 b <- arbitrary
                 let sign = if b then 1.0 else -1.0
                 return $ NonZero (n * maxNumber * sign)

instance coarbNonZero :: CoArbitrary NonZero where
  coarbitrary (NonZero n) = coarbitrary n

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = do
    n <- uniform
    return $ (n * 2) < 1

instance coarbBoolean :: CoArbitrary Boolean where
  coarbitrary true  = perturbGen 1
  coarbitrary false = perturbGen 2

instance arbString :: Arbitrary String where
  arbitrary = do
    arrNum <- arbitrary
    return $ (S.joinWith "") $ S.fromCharCode <<< ((*) 65535) <$> arrNum

instance coarbString :: CoArbitrary String where
  coarbitrary s = coarbitrary $ (S.charCodeAt 0 <$> S.split "" s)

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = do
    arrNum <- arbitrary
    return $ AlphaNumString <<< (S.joinWith "") $ lookup <$> arrNum where
      chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

      lookup x = S.charAt index chars where
        index = round $ x * (S.length chars - 1)

instance coarbAlphaNumString :: CoArbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = Tuple <$> arbitrary <*> arbitrary

instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b) where
  coarbitrary (Tuple a b) = coarbitrary a >>> coarbitrary b

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = do
    b <- arbitrary
    if b then Left <$> arbitrary else Right <$> arbitrary

instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary (Left a)  = coarbitrary a
  coarbitrary (Right b) = coarbitrary b

instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a) where
  arbitrary = do
    b <- arbitrary
    if b then pure Nothing else Just <$> arbitrary

instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a) where
  coarbitrary Nothing = perturbGen 1
  coarbitrary (Just a) = coarbitrary a

instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (map f xs) gen

instance arbArray :: (Arbitrary a) => Arbitrary [a] where
  arbitrary = do
    b <- arbitrary
    if b then return [] else do
      a <- arbitrary
      as <- arbitrary
      return (a : as)

instance coarbArray :: (CoArbitrary a) => CoArbitrary [a] where
  coarbitrary [] = id
  coarbitrary (x : xs) = coarbitrary xs <<< coarbitrary x

instance testableResult :: Testable Result where
  test = return

instance testableBoolean :: Testable Boolean where
  test true = return Success
  test false = return $ Failed "Test returned false"

instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop) where
  test f = do
    t <- arbitrary
    test (f t)