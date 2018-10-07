{- | @treasure-keeper@ supports multiple accounts sharig the same database.
This module containes all nessecary functional to work with 'Account's.
-}
module Treasure.Core.Account
       ( Account (..)
       , prettyAccount
       , testAccounts
       ) where


data Account = Account
    { accountName  :: Text
    , accountNick  :: Text
    , accountEmoji :: Text
    } deriving (Show, Eq)

testAccounts :: [Account]
testAccounts =
    [ Account "Dmitrii" "shersh" "🙂"
    , Account "Veronika" "vrom911" "😉"
    ]

prettyAccount :: Account -> Text
prettyAccount Account{..} = accountEmoji <> " " <> accountNick
