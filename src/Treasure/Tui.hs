module Treasure.Tui
       ( executeTreasure
       ) where

import Brick (App (..), AttrMap, BrickEvent (VtyEvent), Padding (Pad), Widget, attrMap, continue,
              customMain, halt, padTop, str, (<=>))
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form, FormFieldState, checkboxField, focusedFormInputAttr, formFocus,
                    handleFormEvent, invalidFormInputAttr, newForm, renderForm)

import Treasure.Core.Account (Account, prettyAccount)
import Treasure.Tui.Chest (CheckBox (..), TreasureChest (..), checkBoxL, chestAccountsL,
                           initialTreasureChest)

import qualified Brick (on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V


data MainBox
    = AccountsField Int
    deriving (Eq, Ord, Show)


-- Creates the inout form from the given initial 'TreasureChest'.
mkForm :: forall e . TreasureChest -> Form TreasureChest e MainBox
mkForm t = newForm (map makeCheckBox $ zip [0..] $ chestAccounts t) t
  where
    makeCheckBox
        :: (Int, CheckBox Account)
        -> TreasureChest
        -> FormFieldState TreasureChest e MainBox
    makeCheckBox (i, CheckBox{..}) = checkboxField
        (chestAccountsL . checkBoxL i)
        (AccountsField i)
        (prettyAccount checkboxData)

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
