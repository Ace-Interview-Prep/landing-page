module Landing.Pages.ContactUsForm where

import Classh
import Classh.Reflex
import Reflex.Dom
import Control.Monad (void)
import qualified Data.Text as T

type Shell m a = m a -> m a


editorCanvas' :: DomBuilder t m => Shell m a
editorCanvas' dom = do 
  placeCenterWidth [("def", pct 50)] $ do
    el "x" $ do
      elClass "div" "bg-gray-200" $ do
        dom


contactUsFormPage :: DomBuilder t m => m ()
contactUsFormPage = do
  editorCanvas' contactUsForm


-- -- | A generic interface to creating a centered row of some width+width constraints
-- rowH :: DomBuilder t m => TWSizeOrFraction -> (Justify, Align) -> m a -> m a 
-- rowH heightCfg pos_ = centerSimple . divClass (classhUnsafe [ h .~~ heightCfg ])


--inputEl :: [Mutation BoxConfig] ->
--inputEl

inputEl :: DomBuilder t m => T.Text -> T.Text -> m () 
inputEl elClasses textClasses = void $ inputElement $ def & inputElementConfig_elementConfig . initialAttributes .~
  ("class" =: (elClasses <&> textClasses))

textAreaEl :: DomBuilder t m => T.Text -> T.Text -> m () 
textAreaEl elClasses textClasses = void $ textAreaElement $ def & textAreaElementConfig_elementConfig . initialAttributes .~
  ("class" =: (elClasses <&> textClasses))

  
contactUsForm :: DomBuilder t m => m ()
contactUsForm = el "div" $ do
  --let textBox = text "heyY"
  
  elClass "div" "p-16" $ centerSimple $ do
    row [y .~~ TWSize 10] $ do
      inputEl $(classh' [w .~~ pct 90]) ""
      --void $ inputElement $ def & inputElementConfig_elementConfig . initialAttributes .~ ("class" =: "")
    gridCol Col12 $ do
      col [6] $ inputEl $(classh' [w .~~ pct 90]) ""
      col [6] $ inputEl $(classh' [w .~~ pct 90]) ""
    row [y .~~ TWSize 10] $ do
      textAreaEl $(classh' [w .~~ pct 90]) ""
  el "div" $ do
    text "heyYY"
    _ <- button "click"
    pure ()
