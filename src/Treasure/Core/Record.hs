module Treasure.Core.Record
       ( Record (..)

       , Tag (..)
       , mkTag
       , testTags

       , Amount (..)
       ) where

import Treasure.Core.Account (Account)

import qualified Data.Text as T


{- | Expenses record.
-}
data Record = Record
    { -- | Account from what we're spending money
      recordAccountFrom :: Account

      -- | Tags for the spent amount of money.
    , recordTags        :: NonEmpty Tag

      -- | Amount of spent many.
    , recordAmount      :: Amount
    }

-- | Expense record tag. Can be used for search.
newtype Tag = Tag
    { unTag :: Text
    } deriving newtype (Show)

testTags :: [Tag]
testTags = [Tag "Food", Tag "Utilities"]

mkTag :: Text -> Maybe Tag
mkTag tag
    | T.null tag = Nothing
    | otherwise  = Just (Tag tag)

newtype Amount = Amount
    { unAmount :: Rational
    } deriving newtype (Eq, Ord, Num)
