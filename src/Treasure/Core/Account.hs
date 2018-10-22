{- | @treasure-keeper@ supports multiple accounts sharig the same database.
This module containes all nessecary functional to work with 'Account's.
-}
module Treasure.Core.Account
       ( Account (..)

       , testAccounts
       ) where

import qualified Text.Show as Show

data Account = Account
    { accountName  :: Text
    , accountNick  :: Text
    , accountEmoji :: Text
    } deriving (Eq)

instance Show Account where
    show = toString . accountNick

testAccounts :: [Account]
testAccounts =
    [ Account "Dmitrii" "shersh" "🙂"
    , Account "Veronika" "vrom911" "😉"
    ]
