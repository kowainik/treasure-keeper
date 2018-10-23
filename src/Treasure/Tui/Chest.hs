{-# LANGUAGE Rank2Types #-}

{- | This module contains data types to work with application form.
'TreasureChest' is the data type containing the values manipulated by the fields
in the form.
-}

module Treasure.Tui.Chest
       ( -- * Data types
         TreasureChest (..)
       , initialTreasureChest

         -- * Check boxes
       , CheckBox (..)
       , toCheckBoxes

         -- * Lenses
       , chestAccountsL
       , chestTagsL
       , checkBoxL
       ) where

import Lens.Micro (Lens', ix, lens, (.~))

import Treasure.Core.Account (Account, testAccounts)
import Treasure.Core.Record (Tag, testTags)

import qualified Relude.Unsafe as Unsafe


-- | Represents the checkbox for some data.
data CheckBox a = CheckBox
    { checkboxData :: a
    , checkboxFlag :: Bool
    } deriving (Show)

-- | Creates the list of the 'CheckBox'es from the list of data with the 'False'
-- flags.
toCheckBoxes :: [a] -> [CheckBox a]
toCheckBoxes = map (`CheckBox` False)

-- | Lens for 'checkboxFlag' field of the 'CheckBox' data type.
flagL :: Lens' (CheckBox a) Bool
flagL = lens checkboxFlag $ \checkbox newFlag -> checkbox { checkboxFlag = newFlag }

{- |
-}
checkBoxL :: Int -> Lens' [CheckBox a] Bool
checkBoxL i = lens getAt setAt
  where
    getAt :: [CheckBox a] -> Bool
    getAt l = checkboxFlag $ Unsafe.at i l

    setAt :: [CheckBox a] -> Bool -> [CheckBox a]
    setAt l newBool = l & ix i . flagL .~ newBool


-- | Global TUI state.
data TreasureChest = TreasureChest
    { chestAccounts :: [CheckBox Account]
    , chestTags     :: [CheckBox Tag]
    } deriving (Show)

-- | Lens for 'chestAccounts' field of the 'TreasureChest'.
chestAccountsL :: Lens' TreasureChest [CheckBox Account]
chestAccountsL = lens chestAccounts $ \chest newAccounts ->
    chest { chestAccounts = newAccounts }

-- | Lens for 'chestTags' field of the 'TreasureChest'.
chestTagsL :: Lens' TreasureChest [CheckBox Tag]
chestTagsL = lens chestTags $ \chest newTags ->
    chest { chestTags = newTags }

-- | Initial global state of the tui.
initialTreasureChest :: TreasureChest
initialTreasureChest = TreasureChest
    { chestAccounts = toCheckBoxes testAccounts
    , chestTags = toCheckBoxes testTags
    }
