{-# LANGUAGE Rank2Types #-}

{- | This module contains data types to work with application form.
'TreasureChest' is the data type containing the values manipulated by the fields
in the form.
-}

module Treasure.Tui.Chest
       ( -- * Data types
         TreasureChest (..)
       , CheckBox (..)
       , toCheckBoxes

         -- * Lenses
       , chestAccountsL
       , checkBoxL
       ) where

import Control.Lens (Lens', ix, lens, (.~))

import Treasure.Core.Account (Account)

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
flag :: Lens' (CheckBox a) Bool
flag = lens checkboxFlag $ \checkbox newFlag -> checkbox { checkboxFlag = newFlag }

{- |
-}
checkBoxL :: Int -> Lens' [CheckBox a] Bool
checkBoxL i = lens getAt setAt
  where
    getAt :: [CheckBox a] -> Bool
    getAt l = checkboxFlag $ Unsafe.at i l

    setAt :: [CheckBox a] -> Bool -> [CheckBox a]
    setAt l newBool = l & ix i . flag .~ newBool


-- | Global TUI state.
data TreasureChest = TreasureChest
    { chestAccounts :: [CheckBox Account]
    } deriving (Show)

-- | Lens for 'chestAccounts' field of the 'TreasureChest'.
chestAccountsL :: Lens' TreasureChest [CheckBox Account]
chestAccountsL = lens chestAccounts $ \chest newAccounts ->
    chest { chestAccounts = newAccounts }
