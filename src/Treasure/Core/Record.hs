module Treasure.Core.Record
       ( Record (..)

       , Tag (..)
       , mkTag

       , Amount (..)
       ) where

import qualified Data.Text as T

{- | Expenses record.
-}
data Record = Record
    { -- | Account from what we're spending money
      recordAccountFrom :: Undefined

      -- | Tags for the spent amount of money.
    , recordTags        :: NonEmpty Tag

      -- | Amount of spent many.
    , recordAmount      :: Amount
    }

-- | Expense record tag. Can be used for search.
newtype Tag = Tag
    { unTag :: Text
    }

mkTag :: Text -> Maybe Tag
mkTag tag
    | T.null tag = Nothing
    | otherwise  = Just (Tag tag)

newtype Amount = Amount
    { unAmount :: Ratio Natural
    }
