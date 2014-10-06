module Test.QuickCheck.LCG
  ( GenT()
  , Gen()
  , GenState()
  , GenOut()
  , Size()
  , LCG()
  , repeatable
  , stateful
  , variant
  , sized
  , resize
  , choose
  , chooseInt
  , oneOf
  , frequency
  , listOf
  , listOf1
  , vectorOf
  , elements
  , evalGen
  , perturbGen
  , uniform
  , showSample
  , showSample'
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Debug.Trace
import Data.Tuple
import Data.Lazy
import Data.Profunctor
import Data.Monoid
import Data.Monoid.Sum
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Foldable
import Data.Traversable
import qualified Data.Array as A
import Control.Monad.Free
import Control.Monad.Trampoline
import Control.Arrow
import Control.Monad
import Control.Bind
import qualified Math as M
import qualified Data.Machine.Mealy as Mealy

type Size = Number
type LCG  = Number

data GenState = GenState { seed :: Number, size :: Number }

runGenState :: GenState -> { seed :: Number, size :: Number }
runGenState (GenState s) = s

stateM :: ({ seed :: Number, size :: Number } -> { seed :: Number, size :: Number }) -> GenState -> GenState
stateM f = GenState <<< f <<< runGenState

data GenOut a = GenOut { state :: GenState, value :: a }

runGenOut :: forall a. GenOut a -> { state :: GenState, value :: a }
runGenOut (GenOut v) = v

data GenT f a = GenT (Mealy.MealyT f GenState (GenOut a))

data Gen a = Gen (GenT (Free Lazy) a)

runGen :: forall f a. GenT f a -> Mealy.MealyT f GenState (GenOut a)
runGen (GenT m) = m

lcgM :: Number
lcgM = 1103515245 

lcgC :: Number
lcgC = 12345

lcgN :: Number
lcgN = 1 `shl` 30

foreign import almostOne "var AlmostOne = 1 - Number.EPSILON;" :: Number

lcgNext :: Number -> Number
lcgNext n = (lcgM * n + lcgC) % lcgN

perturbState :: GenState -> GenState
perturbState (GenState s) = GenState { seed: lcgNext s.seed, size: s.size }

lcgStep :: forall f. (Monad f) => GenT f Number
lcgStep = GenT $ arr $ \s -> GenOut { state: perturbState s, value: (runGenState s).seed } 

uniform :: forall f. (Monad f) => GenT f Number
uniform = (\n -> n / (1 `shl` 30)) <$> lcgStep

_foo :: forall f a. Mealy.Step f s a -> f (Maybe a)
_foo Mealy.Halt        = Mealy.Emit Nothing Mealy.Halt
_foo (Mealy.Emit a _)  = Mealy.Emit (Just $ (runGenOut a).value) Mealy.Halt

evalGen' :: forall f a. (Monad f) => GenT f a -> GenState -> f (Maybe a) 
evalGen' (GenT g) st = _foo <$> Mealy.stepMealy st g

evalGen :: forall f a. (Monad f) => GenT f a -> GenState -> f a
evalGen g s = fromJust <$> evalGen' g s

pureGen :: forall f a. (Monad f) => (GenState -> GenOut a) -> GenT f a
pureGen f = GenT $ arr f

repeatable :: forall f a b. (Monad f) => (a -> GenT f b) -> GenT f (a -> f b)
repeatable f = GenT $ let next = Mealy.pureMealy $ \s -> Mealy.Emit (GenOut { state: s, value: \a -> evalGen (f a) s }) next
                      in  next

stateful :: forall f a. (Monad f) => (GenState -> GenT f a) -> GenT f a
stateful f = GenT $ do s <- id 
                       runGen (f s)

variant :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
variant n g = GenT $ lmap (stateM (\s -> s { seed = n })) (runGen g)

sized :: forall f a. (Monad f) => (Number -> GenT f a) -> GenT f a
sized f = stateful $ \s -> f (runGenState s).size

resize :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
resize sz g = GenT $ lmap (stateM (\s -> s { size = sz })) (runGen g)

choose :: forall f. (Monad f) => Number -> Number -> GenT f Number
choose a b = (*) (max - min) >>> (+) min <$> uniform 
  where min = M.min a b
        max = M.max a b

chooseInt :: forall f. (Monad f) => Number -> Number -> GenT f Number
chooseInt a b = M.floor <$> choose (M.ceil a) (M.floor b + almostOne)

oneOf :: forall f a. (Monad f) => GenT f a -> [GenT f a] -> GenT f a
oneOf x xs = do n <- chooseInt 0 (A.length xs)
                if n == 0 then x else fromMaybe x (xs A.!! (n - 1))

frequency :: forall f a. (Monad f) => Tuple Number (GenT f a) -> [Tuple Number (GenT f a)] -> GenT f a
frequency x xs = 
  let xxs   = x : xs
      total = runSum $ fold (((Sum <<< fst) <$> xxs) :: [Sum])
      pick n d [] = d
      pick n d ((Tuple k x) : xs) = if n <= k then x else pick (n - k) d xs

  in do n <- chooseInt 1 total
        pick n (snd x) xxs

listOf :: forall f a. (Monad f) => GenT f a -> GenT f [a]
listOf g = sized $ \n -> 
  do k <- chooseInt 0 n
     vectorOf k g

listOf1 :: forall f a. (Monad f) => GenT f a -> GenT f (Tuple a [a])
listOf1 g = sized $ \n ->
  do k  <- chooseInt 0 n
     x  <- g
     xs <- vectorOf (k - 1) g
     return $ Tuple x xs

vectorOf :: forall f a. (Monad f) => Number -> GenT f a -> GenT f [a]
vectorOf k g = sequence $ const g <$> (A.range 1 k)

elements :: forall f a. (Monad f) => a -> [a] -> GenT f a
elements x xs = do
  n <- chooseInt 0 (A.length xs)
  pure if n == 0 then x else fromMaybe x (xs A.!! (n - 1))

foreign import float32ToInt32 
  "function float32ToInt32(n) {\
  \  var arr = new ArrayBuffer(4);\
  \  var fv = new Float32Array(arr);\
  \  var iv = new Int32Array(arr);\
  \  fv[0] = n;\
  \  return iv[0];\
  \}" :: Number -> Number

perturbGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
perturbGen n (GenT g) = GenT $ lmap (stateM (\s -> s { seed = lcgNext (float32ToInt32 n) + s.seed })) g

liftMealy :: forall f a. (Monad f) => (Mealy.MealyT f GenState (GenOut a) -> Mealy.MealyT f GenState (GenOut a)) -> (GenT f a -> GenT f a)
liftMealy f = \g -> GenT $ f (runGen g)

take :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
take n = liftMealy $ Mealy.take n

drop :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
drop n = liftMealy $ Mealy.drop n

loop :: forall f a. (Monad f) => GenT f a -> GenT f a
loop = liftMealy $ Mealy.loop

sample :: forall f a. (Monad f) => Number -> GenT f a -> f [a]
sample sz g = evalGen (vectorOf sz g) (GenState { seed: 0, size: sz })

showSample' :: forall r a. (Show a) => Size -> Gen a -> Eff (trace :: Trace | r) Unit
showSample' n g = print $ runTrampoline $ sample n g

showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit
showSample = showSample' 10

instance semigroupGenState :: Semigroup GenState where
  (<>) (GenState a) (GenState b) = GenState { seed: a.seed * b.seed, size: b.size }

instance monoidGenState :: Monoid GenState where
  mempty = GenState { seed: 0, size: 10 }

instance functorGenOut :: Functor GenOut where
  (<$>) f (GenOut m) = GenOut { state: m.state, value: f m.value }

-- GenT instances
instance functorGenT :: (Monad f) => Functor (GenT f) where
  (<$>) f (GenT m) = GenT $ (<$>) f <$> m

instance applyGenT :: (Monad f) => Apply (GenT f) where
  (<*>) (GenT f) (GenT x) = GenT $ do GenOut { state = s1, value = f } <- f
                                      GenOut { state = s2, value = x } <- x
                                      return $ GenOut { state: s1 <> s2, value: f x }

instance applicativeGenT :: (Monad f) => Applicative (GenT f) where
  pure t = GenT $ arr (\s -> GenOut { state: perturbState s, value: t })

instance semigroupGenT :: (Monad f) => Semigroup (GenT f a) where
  (<>) (GenT a) (GenT b) = GenT (a <> b)

instance monoidGenT :: (Monad f) => Monoid (GenT f a) where
  mempty = GenT mempty

instance bindGenT :: (Monad f) => Bind (GenT f) where
  (>>=) (GenT m) f = GenT $ do GenOut { state = s1, value = a } <- m
                               GenOut { state = s2, value = b } <- runGen $ f a
                               return $ GenOut { state: s1 <> s2, value: b }

instance monadGenT :: (Monad f) => Monad (GenT f)

-- Gen instances
instance functorGen :: Functor Gen where
  (<$>) f (Gen m) = Gen $ f <$> m

instance applyGen :: Apply f where
  (<*>) (Gen f) (Gen x) = Gen $ f <*> x

instance applicativeGen :: Applicative Gen where
  pure t = Gen $ pure t

instance semigroupGen :: Semigroup (Gen a) where
  (<>) (Gen x) (Gen y) = Gen (x <> y)

instance monoidGen :: Monoid (Gen a) where
  mempty = Gen mempty

instance bindGen :: Bind Gen where
  (>>=) (Gen m) f = Gen $ m >>= f

instance monadGen :: Monad Gen