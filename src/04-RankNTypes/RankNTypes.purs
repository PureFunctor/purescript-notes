module RankNTypes.RankNTypes where

import Data.Tuple (Tuple(..))
import Prim.Row (class Cons)


-- Rank N Types can be used to hide details from the user,
-- a simple example would be a `Cons'` type that simply
-- abstracts away a parameter from the `Cons` constraint
-- when dealing with records.
data Cons' (l :: Symbol) (t :: Type) (_1 :: # Type) = Cons' (forall s. Cons l t _1 s => { | s})


unwrapCons' :: Cons' "y" String (x :: Int) -> Tuple Int String
unwrapCons' (Cons' s) = Tuple s.x s.y
