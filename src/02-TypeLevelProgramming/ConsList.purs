module TypeLevelProgramming.ConsList where

import Prelude

import Prim.Boolean (False, True, kind Boolean)


-- This module implements a type-level list of types that
-- supports compile-time membership testing.


-- The kind for all type-level lists.
foreign import kind List'

-- The base case, represents an empty list.
foreign import data Nil' :: List'

-- Higher-kinded type-level value that prepends to a
-- type-level list, producing a new type-level list.
foreign import data Cons' :: Type -> List' -> List'

-- Infix synonym for easier construction of types.
infixr 1 type Cons' as :


-- A typeclass for membership testing
class Member ( x :: Type ) ( xs :: List' ) ( r :: Boolean ) | x xs -> r


-- Some type x is never a member of an empty list.
instance memberNil ::
  Member x Nil' False

else

-- Some type x is the same as the leftmost element.
instance memberCons ::
  Member x ( x : xs ) True

else

-- Some type x is not the same as the leftmost element,
-- invoking a recursive search using the `Member`
-- constraint that resolves the result `r`.
instance memberRec
  :: Member x ys r
  => Member x ( y : ys ) r


-- Different proxy types for working with these kinds.
data BProxy ( b :: Boolean ) = BProxy
data LProxy ( l :: List' ) = LProxy
data TProxy ( t :: Type ) = TProxy


-- Examples
common :: LProxy ( Int : String : Nil' )
common = LProxy


-- Just membership tests.
member :: forall x xs r. Member x xs r => TProxy x -> LProxy xs -> BProxy r
member _ _ = BProxy

yes :: BProxy True
yes = member ( TProxy :: TProxy Int ) common

no :: BProxy False
no = member ( TProxy :: TProxy Char ) common


-- Compile-time restrictions based on membership tests.
restricted :: forall x xs. Member x xs True => TProxy x -> LProxy xs -> Unit
restricted _ _ = unit

success :: Unit
success = restricted ( TProxy :: TProxy Int ) common


-- failure :: Unit
-- failure = restricted ( TProxy :: TProxy Char ) common


-- Prepending more types.
cons :: forall x xs. TProxy x -> LProxy xs -> LProxy ( x : xs )
cons _ _ = LProxy

common' :: LProxy ( Char : Int : String : Nil' )
common' = cons ( TProxy :: TProxy Char ) common
