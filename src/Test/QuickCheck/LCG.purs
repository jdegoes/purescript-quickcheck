module Test.QuickCheck.LCG
  ( GenT(..)
  , Gen(..)
  , GenState(..)
  , GenOut(..)
  , Size()
  , LCG()
  , arrayOf 
  , arrayOf1 
  , choose 
  , chooseInt 
  , dropGen 
  , elements 
  , foldGen 
  , frequency 
  , fromArray
  , loopGen 
  , oneOf 
  , perturbGen 
  , repeatable 
  , resize 
  , sample 
  , sample' 
  , showSample
  , showSample' 
  , sized 
  , stateful 
  , suchThat
  , suchThatMaybe
  , takeGen 
  , unfoldGen 
  , uniform 
  , variant 
  , vectorOf 
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

unGenState :: GenState -> { seed :: Number, size :: Number }
unGenState (GenState s) = s

stateM :: ({ seed :: Number, size :: Number } -> { seed :: Number, size :: Number }) -> GenState -> GenState
stateM f = GenState <<< f <<< unGenState

data GenOut a = GenOut { state :: GenState, value :: a }

unGenOut :: forall a. GenOut a -> { state :: GenState, value :: a }
unGenOut (GenOut v) = v

data GenT f a = GenT (Mealy.MealyT f GenState (GenOut a))

type Gen a = GenT Trampoline a

unGen :: forall f a. GenT f a -> Mealy.MealyT f GenState (GenOut a)
unGen (GenT m) = m

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
lcgStep = GenT $ arr $ \s -> GenOut { state: perturbState s, value: (unGenState s).seed } 

uniform :: forall f. (Monad f) => GenT f Number
uniform = (\n -> n / (1 `shl` 30)) <$> lcgStep

stepGen :: forall f a. (Monad f) => GenState -> GenT f a -> f (Maybe (GenOut (Tuple a (GenT f a))))
stepGen st (GenT m) =   h <$> Mealy.stepMealy st m
                        where h Mealy.Halt        = Nothing
                              h (Mealy.Emit a m)  = Just $ flip Tuple (GenT m) <$> a 

evalGen' :: forall f a. (Monad f) => GenT f a -> GenState -> f (Maybe a) 
evalGen' g st = h <$> stepGen st g
                where h Nothing                               = Nothing
                      h (Just (GenOut { value = Tuple a _ })) = Just a

pureGen :: forall f a. (Monad f) => (GenState -> GenOut a) -> GenT f a
pureGen f = GenT $ arr f

repeatable' :: forall f a b. (Monad f) => (a -> GenT f b) -> GenT f (a -> f b)
repeatable' f = GenT $ 
  let next = Mealy.pureMealy $ \s -> Mealy.Emit (GenOut { state: s, value: \a -> fromJust <$> evalGen' (f a) s }) next
  in  next

repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = g <$> repeatable' f
               where g f' = \a -> runTrampoline $ f' a

stateful :: forall f a. (Monad f) => (GenState -> GenT f a) -> GenT f a
stateful f = GenT $ do s <- id 
                       unGen (f s)

variant :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
variant n g = GenT $ lmap (stateM (\s -> s { seed = n })) (unGen g)

sized :: forall f a. (Monad f) => (Number -> GenT f a) -> GenT f a
sized f = stateful $ \s -> f (unGenState s).size

resize :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
resize sz g = GenT $ lmap (stateM (\s -> s { size = sz })) (unGen g)

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

arrayOf :: forall f a. (Monad f) => GenT f a -> GenT f [a]
arrayOf g = sized $ \n -> 
  do k <- chooseInt 0 n
     vectorOf k g

arrayOf1 :: forall f a. (Monad f) => GenT f a -> GenT f (Tuple a [a])
arrayOf1 g = sized $ \n ->
  do k  <- chooseInt 0 n
     x  <- g
     xs <- vectorOf (k - 1) g
     return $ Tuple x xs

vectorOf :: forall f a. (Monad f) => Number -> GenT f a -> GenT f [a]
vectorOf n = unfoldGen f []
  where f b a = let b' = b <> [a] 
                in  if A.length b' >= n then Tuple [] (Just b') else Tuple b' Nothing

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
liftMealy f = \g -> GenT $ f (unGen g)

takeGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
takeGen n = liftMealy $ Mealy.take n

dropGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
dropGen n = liftMealy $ Mealy.drop n

loopGen :: forall f a. (Monad f) => GenT f a -> GenT f a
loopGen = liftMealy $ Mealy.loop

foldGen :: forall f a b. (Monad f) => (b -> a -> Maybe b) -> b -> GenState -> GenT f a -> f b
foldGen f b s (GenT m) = loop s m b where
  loop st m b = Mealy.stepMealy st m >>= g
    where g Mealy.Halt                                        = pure b
          g (Mealy.Emit (GenOut { value = a, state = st }) m) = let b' = f b a in maybe (pure b) (loop st m) b'

unfoldGen :: forall f a b c. (Monad f) => (b -> a -> Tuple b (Maybe c)) -> b -> GenT f a -> GenT f c
unfoldGen f b (GenT m) = GenT $ loop m b where
  loop m b = Mealy.mealy $ \st -> Mealy.stepMealy st m >>= g
    where g Mealy.Halt                                        = pure Mealy.Halt
          g (Mealy.Emit (GenOut { value = a, state = st }) m) = case f b a of 
                                                                  Tuple b Nothing  -> Mealy.stepMealy st (loop m b)
                                                                  Tuple b (Just c) -> 
                                                                    let c' = GenOut { value: c, state: st }
                                                                    in  pure $ Mealy.Emit c' (loop m b)

-- FIXME: workaround type inference unification bug
ifThenElse p a b = if p then a else b

suchThat :: forall f a. (Monad f) => GenT f a -> (a -> Boolean) -> GenT f a
suchThat g p = unfoldGen f unit g where
  f _ a = Tuple unit $ ifThenElse (p a) (Just a) Nothing

suchThatMaybe :: forall f a. (Monad f) => Number -> GenT f a -> (a -> Boolean) -> GenT f (Maybe a)
suchThatMaybe n g p = unfoldGen f 0 g where
  f i a = ifThenElse (p a) (Tuple 0 (Just $ Just a)) (ifThenElse (i >= n) (Tuple 0 (Just $ Nothing)) (Tuple (i + 1) Nothing))

fromArray :: forall f a. (Monad f) => [a] -> GenT f a
fromArray a = GenT $ Mealy.fromArray a

sample' :: forall f a. (Monad f) => Number -> GenState -> GenT f a -> f [a]
sample' n = foldGen f []
  where f v a = ifThenElse (A.length v < n) (Just $ v <> [a]) Nothing 

sample :: forall f a. (Monad f) => Number -> GenT f a -> f [a]
sample n = sample' n (GenState { size: 10, seed: 0 })

showSample' :: forall r a. (Show a) => Number -> Gen a -> Eff (trace :: Trace | r) Unit
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
                               GenOut { state = s2, value = b } <- unGen $ f a
                               return $ GenOut { state: s1 <> s2, value: b }

instance monadGenT :: (Monad f) => Monad (GenT f)