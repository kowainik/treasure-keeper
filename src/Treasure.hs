module Treasure
       ( openTreasureChest
       ) where

import Brick.Forms (formState)

import Treasure.Tui (executeTreasure)
import Treasure.Tui.Chest (initialTreasureChest)


openTreasureChest :: IO ()
openTreasureChest = do
    tkForm <- executeTreasure

    putTextLn $ "The starting form state was:" <> show initialTreasureChest
    putTextLn $ "The final form state was:" <> show (formState tkForm)
