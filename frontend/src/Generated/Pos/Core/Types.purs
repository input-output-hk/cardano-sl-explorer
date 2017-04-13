-- File auto generated by purescript-bridge! --
module Pos.Core.Types where

import Data.Lens (Lens', Prism', lens, prism')
import Data.Maybe (Maybe(..))
import Prim (Int)

import Prelude
import Data.Generic (class Generic)

newtype Coin =
    Coin {
      getCoin :: Int
    }

derive instance genericCoin :: Generic Coin

--------------------------------------------------------------------------------
_Coin :: Prism' Coin { getCoin :: Int}
_Coin = prism' Coin f
  where
    f (Coin r) = Just r


--------------------------------------------------------------------------------
newtype EpochIndex =
    EpochIndex {
      getEpochIndex :: Int
    }

derive instance genericEpochIndex :: Generic EpochIndex

--------------------------------------------------------------------------------
_EpochIndex :: Prism' EpochIndex { getEpochIndex :: Int}
_EpochIndex = prism' EpochIndex f
  where
    f (EpochIndex r) = Just r


--------------------------------------------------------------------------------
newtype LocalSlotIndex =
    LocalSlotIndex {
      getSlotIndex :: Int
    }

derive instance genericLocalSlotIndex :: Generic LocalSlotIndex

--------------------------------------------------------------------------------
_LocalSlotIndex :: Prism' LocalSlotIndex { getSlotIndex :: Int}
_LocalSlotIndex = prism' LocalSlotIndex f
  where
    f (LocalSlotIndex r) = Just r


--------------------------------------------------------------------------------