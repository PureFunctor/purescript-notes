module MonadsAndFriends.FreeDSLs where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Effect (Effect)
import Effect.Console (log)


-- | Theoretically speaking, free monads are basically just
-- | fixed-point functors e.g. `f (f (f (f (...))))` that
-- | form the concept of sequential and lazy computation.
--
-- | If we look at the definition of the `Monad` type class:
--
-- | class Monad m where
-- |   return :: a -> m a
--
-- |   (>>=) :: m a -> (a -> m b) -> m b
-- |   m >>= f = join $ fmap f m
--
-- |   join :: m (m a) -> m a
-- |   join m = m >>= identity
--
-- | We can observe two familiar patterns, namely with the
-- | default definition for `(>>=)` and the type signature
-- | for `join`.
--
-- | Regarding the latter, we can see how it "collapses" a
-- | stack of the same monad, yielding a monadic value. If
-- | we were to interpret this based on our monad intuition
-- | of "computation within contexts", we can say that `join`
-- | produces a monadic value from some context in that same
-- | monad category. At the value level, this also implies
-- | that the monadic value that was produced has access to
-- | the information that the monad it came from provides.
-- | This intuition becomes more apparent if we see the
-- | default definition for `(>>=)`: given a monad, and a
-- | function that produces another monad from the value of
-- | the first monad, it produces a new monad by first
-- | `fmap`-ping the function to the monad, before having
-- | it be `join`-ed and the context be collapsed where
-- | context usually means some operation.
--
-- | But then, how are free monads, monads? We'll get back
-- | to that in a bit, first, let's define free monoids
-- | categorically.
--
-- | Monoids are sets that have an associative binary operation
-- | and an identity element. By taking a "forgetful functor"
-- | that transforms a monoid into a set, and a "free functor"
-- | that transforms a set into a specialized monoid type, by
-- | forming an adjunction between the "free" and "forgetful"
-- | functors, we essentially end up with the notion of "free"
-- | monoids, something that we can recursively stack through
-- | the "free" functor and unwrap through the "forgetful"
-- | functor.
--
-- | One such example of a free monoid is a list, where our
-- | "free functor" manifests its left adjunction in the
-- | form of `foldMap` with the following signature:
-- |
-- | foldMap ::
-- |   forall a m. Monoid m => (a -> m) -> List a -> m
-- |                                         \
-- |                                          F a -> m
-- |
-- | With this in mind, we would know that our "forgetful
-- | functor", would have the form of `a -> CoList m`.
--
-- | We also know that the definition of lists are recursive
-- | in nature, and are typically defined in terms of "nil"
-- | and "cons", with the latter being recursive.
--
-- | In terms of monads, we can specialize our "forgetful"
-- | functor to turn monads into functors, and our "free"
-- | functor to turn functors into monads. What this means is
-- | that we can recursively stack through the "free" functor
-- | and unwrap through the "forgetful" functor.
--
-- | If we were to define a free monad using a free functor,
-- | we can incorporate our list-as-a-free-monoid example in
-- | order to build such a definition:
-- |
-- | data Free f a = Pure a | Next (f (Free f a))
-- |
-- | This makes it so that `Free` is our "free functor", turning
-- | some functor `f` into a monad by modelling sequential
-- | computation in terms of fixed point that arises from the
-- | use of `Next`.
-- |
-- | We can also infer that our "forgetful functor" would have
-- | the form of `f -> CoFree m`, but that's for another note.
--
-- | Categories out of the way, how should we define free monads?


-- | The first step is to define a fixed-point functor, something
-- | that always will be able to refer to itself through the help
-- | of the `Free` type.
-- |
-- | It's also fair to note that the constructors defined here
-- | follow two different styles, "tell-style" and "request-style".
-- |
-- | A "tell" operates on the arguments given to it without
-- | influencing future computations hence the bare `a` type;
-- | a "request" on the other hand operates on the arguments
-- | given to it while also being able to influence future
-- | computations hence the `(Type -> a)` type.
data Language a
  = Tell String a
  | Request (String -> a)


derive instance functorLanguage :: Functor Language


-- | Next is to create a type alias for the newly created free
-- | monad. By specializing the type aliases as much as possible,
-- | we get the following definition:
-- |
-- | Free Language a = Pure a | Next (Language (Free Language a))
type LanguageF = Free Language


-- | We define helper functions for ease of use.
tell :: String -> LanguageF Unit
tell s = liftF $ Tell s unit


request :: LanguageF String
request = liftF $ Request \s -> s


-- | After which we can start writing monads.
program :: LanguageF Unit
program = do
  tell "Hello, World!"
  name <- request
  tell name


-- | And a runner function that interprets our free monad
-- | into another.
runLanguage :: forall a. LanguageF a -> Effect a
runLanguage = runFreeM go
  where
    go (Tell s a) = log s *> pure a
    go (Request f) = pure (f "Pure")


main :: Effect Unit
main = runLanguage program
