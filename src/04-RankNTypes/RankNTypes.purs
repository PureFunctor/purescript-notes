module RankNTypes.RankNTypes where

import Data.Array (replicate)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons)


-- Rank N Types can be used to hide details from the user,
-- a simple example would be a `Cons'` type that simply
-- abstracts away a parameter from the `Cons` constraint
-- when dealing with records.
data Cons' (l :: Symbol) (t :: Type) (_1 :: # Type) = Cons' (forall s. Cons l t _1 s => { | s})


unwrapCons' :: Cons' "y" String (x :: Int) -> Tuple Int String
unwrapCons' (Cons' s) = Tuple s.x s.y


-- Let's have a bit of fun with type classes, functional
-- dependencies, and Rank N Types.
class RendersTo a b | a -> b , b -> a where
  render :: a -> b


instance intRendersTo :: RendersTo Int (Array Int) where
  render f = replicate f f


instance charRendersTo :: RendersTo Char (Array Char) where
  render f = replicate 10 f


-- We know that Rank N Types can be used to hide details
-- from the user, but this loss of information also
-- affects how the compiler sees these types. Functions
-- making use of ad-hoc polymorphism through type classes
-- are often used in the context of Rank N Types, as they
-- describe morphisms from "any" data into concrete data.
type RenderPair x y =
  { renderToX :: (forall a. RendersTo a x => a -> Tuple a x)
  , renderToY :: (forall a. RendersTo a y => a -> Tuple y a)
  }


-- As such, the user must help the compiler in filling in the
-- details for these different types, which results to the
-- programmer-level restriction of only using concrete types.
pair :: RenderPair (Array Int) (Array Char)
pair =
  { renderToX : \a -> Tuple a (render a)
  , renderToY : \a -> Tuple (render a) a
  }
