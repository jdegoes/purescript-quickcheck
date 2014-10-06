# Module Documentation

## Module Test.QuickCheck.LCG

### Types

    data Gen a

    data GenOut a

    data GenState

    data GenT f a

    type LCG  = Number

    type Size  = Number


### Type Class Instances

    instance applicativeGen :: Applicative Gen

    instance applicativeGenT :: (Monad f) => Applicative (GenT f)

    instance applyGen :: Apply f

    instance applyGenT :: (Monad f) => Apply (GenT f)

    instance bindGen :: Bind Gen

    instance bindGenT :: (Monad f) => Bind (GenT f)

    instance functorGen :: Functor Gen

    instance functorGenOut :: Functor GenOut

    instance functorGenT :: (Monad f) => Functor (GenT f)

    instance monadGen :: Monad Gen

    instance monadGenT :: (Monad f) => Monad (GenT f)

    instance monoidGen :: Monoid (Gen a)

    instance monoidGenState :: Monoid GenState

    instance monoidGenT :: (Monad f) => Monoid (GenT f a)

    instance semigroupGen :: Semigroup (Gen a)

    instance semigroupGenState :: Semigroup GenState

    instance semigroupGenT :: (Monad f) => Semigroup (GenT f a)


### Values

    choose :: forall f. (Monad f) => Number -> Number -> GenT f Number

    chooseInt :: forall f. (Monad f) => Number -> Number -> GenT f Number

    elements :: forall f a. (Monad f) => a -> [a] -> GenT f a

    evalGen :: forall f a. (Monad f) => GenT f a -> GenState -> f a

    frequency :: forall f a. (Monad f) => Tuple Number (GenT f a) -> [Tuple Number (GenT f a)] -> GenT f a

    listOf :: forall f a. (Monad f) => GenT f a -> GenT f [a]

    listOf1 :: forall f a. (Monad f) => GenT f a -> GenT f (Tuple a [a])

    oneOf :: forall f a. (Monad f) => GenT f a -> [GenT f a] -> GenT f a

    perturbGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

    repeatable :: forall f a b. (Monad f) => (a -> GenT f b) -> GenT f (a -> f b)

    resize :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

    showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit

    showSample' :: forall r a. (Show a) => Size -> Gen a -> Eff (trace :: Trace | r) Unit

    sized :: forall f a. (Monad f) => (Number -> GenT f a) -> GenT f a

    stateful :: forall f a. (Monad f) => (GenState -> GenT f a) -> GenT f a

    uniform :: forall f. (Monad f) => GenT f Number

    variant :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

    vectorOf :: forall f a. (Monad f) => Number -> GenT f a -> GenT f [a]



