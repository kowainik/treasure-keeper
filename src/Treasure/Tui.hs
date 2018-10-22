{-# LANGUAGE Rank2Types #-}

module Treasure.Tui
       ( executeTreasure
       ) where

import Brick (App (..), AttrMap, BrickEvent (VtyEvent), Padding (Pad), Widget, attrMap, continue,
              customMain, halt, padTop, str, (<=>))
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form, FormFieldState, checkboxField, focusedFormInputAttr, formFocus,
                    handleFormEvent, invalidFormInputAttr, newForm, renderForm)
import Lens.Micro (Lens')

import Treasure.Tui.Chest (CheckBox (..), TreasureChest (..), checkBoxL, chestAccountsL, chestTagsL,
                           initialTreasureChest)
import Treasure.Tui.GroupBorder (groupBorder)

import qualified Brick (on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V


data MainBox
    = AccountsField Int
    | TagsField Int
    deriving (Eq, Ord, Show)

type TreasureField e = FormFieldState TreasureChest e MainBox

-- Creates the inout form from the given initial 'TreasureChest'.
mkForm :: forall e . TreasureChest -> Form TreasureChest e MainBox
mkForm t@TreasureChest{..} = newForm
    ( toCheckBoxGroup "Accounts" chestAccountsL AccountsField chestAccounts
   ++ toCheckBoxGroup "Tags"     chestTagsL     TagsField     chestTags
    )
    t
  where
    toCheckBoxGroup
        :: forall a . Show a
        => String
        -> Lens' TreasureChest [CheckBox a]
        -> (Int -> MainBox)
        -> [CheckBox a]
        -> [TreasureChest -> TreasureField e]
    toCheckBoxGroup groupName chestL field ch = groupBorder groupName
        (map makeCheckBox $ zip [0..] ch)
      where
        makeCheckBox :: (Int, CheckBox a) -> TreasureChest -> TreasureField e
        makeCheckBox (i, CheckBox{..}) = checkboxField
            (chestL . checkBoxL i)
            (field i)
            (show checkboxData)

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (E.editAttr,           V.white `Brick.on` V.black)
    , (E.editFocusedAttr,    V.black `Brick.on` V.yellow)
    , (invalidFormInputAttr, V.white `Brick.on` V.red)
    , (focusedFormInputAttr, V.black `Brick.on` V.yellow)
    ]

draw :: Form TreasureChest e MainBox -> [Widget MainBox]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.borderWithLabel (str "Form") $ padTop (Pad 1) (renderForm f)
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str "- Enter/Esc quit, mouse interacts with fields"

app :: App (Form TreasureChest e MainBox) e MainBox
app = App
    { appDraw = draw
    , appHandleEvent = \s ev -> case ev of
        VtyEvent V.EvResize {}       -> continue s
        VtyEvent (V.EvKey V.KEsc []) -> halt s
        _                            -> handleFormEvent ev s >>= continue
    , appChooseCursor = focusRingCursor formFocus
    , appStartEvent = pure
    , appAttrMap = const theMap
    }

executeTreasure :: IO (Form TreasureChest e MainBox)
executeTreasure = customMain buildVty Nothing app $ mkForm initialTreasureChest
  where
    buildVty :: IO V.Vty
    buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        pure v
