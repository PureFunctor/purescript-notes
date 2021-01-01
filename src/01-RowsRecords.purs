module RowsRecords where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Type.Data.Row (RProxy(..))


-- | Records are a way to define dot-accessible data structures;
-- | essentially, they're typed JS objects that can be extended.
-- |
-- | Since records are essentially mostly type synonyms, they
-- | can easily be inlined. For the compiler, these two values
-- | below are the same.
type RegularPerson = { name :: String , age :: Int }


regularPerson :: RegularPerson
regularPerson = { name : "John" , age : 24 }


regularPerson' :: { name :: String , age :: Int }
regularPerson' = { name : "John" , age : 24 }


-- | "Open" records that can be extended with new fields
-- | can be defined using the following syntax with type
-- | variables and rows:
-- |
-- | Like "closed" records, these can also be inlined.
type Person r = { name :: String , age :: Int | r }

type Programmer = Person ( language :: String )


programmer :: Programmer
programmer = { name : "Guido" , age : 64 , language : "Python" }


programmer' :: Person ( language :: String )
programmer' = { name : "Guido" , age : 64 , language : "Python" }


-- | Polymorphism is also fine for both types.
type ClosedPoly a b = { x :: a , y :: b }
type OpenPoly a b r = { x :: a , y :: b | r}


-- | The PureScript documentation defines rows as:
-- |
-- | A row of types represents an unordered collection of named types,
-- | with duplicates. Duplicate labels have their types collected together
-- | in order, as if in a NonEmptyList. This means that, conceptually, a
-- | row can be thought of as a type-level Map Label (NonEmptyList Type).
-- |
-- | Rows are not of kind Type: they have kind Row k for some kind k, and
-- | so rows cannot exist as a value. Rather, rows can be used in type
-- | signatures to define record types or other type where labelled,
-- | unordered types are useful.
-- |
-- | Like records, they can be open or closed, polymorphic, and inline-able.
type ClosedAttrs h w = ( height :: h , weight :: w )
type OpenAttrs r h w = ( height :: h , weight :: w | r )


-- | Internally, records are defined through the primitive `Record` kind,
-- | with curly braces being syntactic sugar for the following:
type SomeRecord = Record ( foo :: Int , bar :: Int )
type SomeRecord' = { foo :: Int , bar :: Int }


-- | Since rows only exist on the type-level, they can only be operated on
-- | by type-level operations e.g. type families.


-- | Union
type PersonR = ( name :: String , age :: Int , height :: Int , weight :: Int )

-- | For all `given` and `filler` rows.
-- | The `Union` of `given` and `filler` produces a `PersonR`.
-- | Given a record containing the `given` row,
-- | there exists a `Unit` (or any other type for that matter).
mkPerson ::
  forall given filler
  .  Union given filler PersonR
  => Record given
  -> Unit
mkPerson _ = unit


mkPersonSuccess :: Unit
mkPersonSuccess = mkPerson { name : "John" }


-- | Nub
school :: RProxy ( name :: String , age :: Int , level :: Int )
school = RProxy

work :: RProxy ( name :: String , age :: Int , position :: Int )
work = RProxy


type SchoolWork = ( name :: String , age :: Int , level :: Int , position :: Int )


-- | For all `school`, `work`, `merged`, and `nubbed` rows.
-- | The `Union` of `school` and `work` produces `merged`.
-- | The `Nub` of `merged` produces `nubbed`.
-- | Given a proxy that contains `school`,
-- | and a proxy that contains `work`,
-- | there exists a proxy that contains `nubbed`
mergeSchoolWork ::
  forall school work merged nubbed
  .  Union school work merged
  => Nub merged nubbed
  => RProxy school
  -> RProxy work
  -> RProxy nubbed
mergeSchoolWork _ _ = RProxy


-- | For all `r` rows and any `a`.
-- | Given a proxy that contains `r`,
-- | some value `a`,
-- | and a function that turns `a` into a record containing `r` rows,
-- | there exists a record that contains `r` rows.
resolve ::
  forall r a
  .  RProxy r
  -> a
  -> (a -> Record r)
  -> Record r
resolve _ a f = f a


responsibilities :: RProxy SchoolWork
responsibilities = mergeSchoolWork school work


info :: Record SchoolWork
info = resolve responsibilities unit \_ -> { name : "Pure" , age : 17 , level : 12 , position : -1 }


-- | Lacks

-- | For all `possible` rows.
-- | `Lacks` restricts `possible` from `illegal`.
-- | There exists a proxy containing `possible`.
restrict ::
  forall possible
  .  Lacks "illegal" possible
  => RProxy possible
restrict = RProxy


restrictSuccess :: { greeting :: String }
restrictSuccess = resolve restrict unit \_ -> { greeting : "hello, world" }


-- restrictFailing :: { illegal :: String }
-- restrictFailing = resolve restrict unit \_ -> { illegal : "oh no" }


-- | Cons

-- | For all `tail` and `possible` rows.
-- | `Cons` appends the `always :: String` pair into `tail`, producing `possible`.
-- | There exists a proxy containing `possible`.
require ::
  forall tail possible
  .  Cons "always" String tail possible
  => RProxy possible
require = RProxy


requireSuccess :: { greeting :: String , always :: String }
requireSuccess = resolve require unit \_ -> { greeting : "hello, world" , always : "here" }


-- requireFailure :: { greeting :: String }
-- requireFailure = resolve require unit \_ -> { greeting : "hello, world" }


-- | Practical Examples:


-- | Restricting parameters to certain choices on the type level,
-- | similar to how Halogen handles slots and slot queries.


data Ping (spec :: # Type) = Ping


-- | This type signature ensures that the `label` that is passed
-- | alongside `SProxy` is part of the `spec`.
-- |
-- | label : tail == spec
-- |
-- | If `label` is a member of `spec`:
-- |
-- | "foo" : tail == [ "foo" , "bar" , "baz" ]
-- |
-- | The compiler is able to tell that:
-- |
-- | tail == [ "bar" , "baz" ]
-- |
-- | Allowing the types to unify.
-- |
-- | If `label` is _not_ a member of `spec`:
-- |
-- | "oof" : tail == [ "foo" , "bar" , "baz" ]
-- |
-- | The compiler thinks that:
-- |
-- | `tail` is any arbitrary row:
-- |
-- | tail == [...]
-- |
-- | and that `spec` actually is:
-- |
-- | "oof" : [...] == ["oof", ...]
-- |
-- | Causing a compile-time error.
ping ::
  forall label tail spec
  .  Cons label Unit tail spec
  => IsSymbol label
  => SProxy label
  -> Ping spec
ping _ = Ping


type SampleSpec = ( box_one :: Unit , box_two :: Unit )


pingSuccess :: Ping SampleSpec
pingSuccess = ping (SProxy :: SProxy "box_one")


-- pingFailure :: Ping SampleSpec
-- pingFailure = ping (SProxy :: SProxy "box_three")
