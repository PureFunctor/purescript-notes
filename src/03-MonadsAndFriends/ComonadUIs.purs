module MonadsAndFriends.ComonadUIs where

import Prelude

import Control.Comonad (class Comonad, duplicate)
import Control.Extend (class Extend)


data UI state view = UI state (state -> view)


derive instance functorUI :: Functor (UI state)


instance comonadExtend :: Extend (UI state) where
  extend f = map f <<< duplicate


instance comonandUI :: Comonad (UI state) where
  extract (UI state render) = render state


type State = { count :: Int }
type View =  String


ui :: UI State View
ui = UI state render
  where
    state :: State
    state = { count : 0 }

    render :: State -> View
    render { count } = "The current count is: " <> show count
