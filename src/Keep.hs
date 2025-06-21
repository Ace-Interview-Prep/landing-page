-- | A pile of unused but useful as examples code or functions
-- | we need to upstream


{-# LANGUAGE OverloadedStrings #-}

module Landing.Pages.Base where 

import Language.Haskell.TH

import Landing.Pages.Elems 
import Common.Constants
import qualified Common.Elems
import Common.Route
import Templates.Partials.Image
import Landing.Utils
import Lamarckian.Types
import Lamarckian.JS

import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (El)

import Control.Monad.Fix
import Control.Monad (forM, forM_, void)
import qualified Data.Map as Map
import qualified Data.Text as T

import Classh as C
import Classh.Reflex as C


-- "width-8"
-- "w-8"

myNumber :: Int 
myNumber = $( [| 1 + 1 |] )

-- px
-- pr 

-- class MyClass a where
--   id_ :: a -> a 
--- $(createMyClass)

-- | Setting for faster hot reloading
-- relateSourceTree :: Tree SourceFilePath -> Tree HtmlFilePath
-- relateSourceTree = undefined

showSourceFile :: Q Exp
showSourceFile = do
  loc <- location
  let file = loc_filename loc
  [| file |]


type Highlighted = Bool

data Testimonial = Testimonial
  { _testimonial_name :: T.Text
  , _testimonial_jobTitle :: T.Text
  , _testimonial_orgName :: T.Text
  , _testimonial_headshot :: T.Text 
  , _testimonial_texts :: [(Highlighted, T.Text)]
  }
  
data Feature = Prepare | Practice | Progress deriving (Eq, Show)

domEvents
  :: (HasDomEvent t target eventName, Reflex t)
  => [EventName eventName]
  -> target
  -> Event t (DomEventType target eventName)
domEvents eNames e = leftmost $ fmap (\eName -> domEvent eName e) eNames 


splitTriplePure :: Functor (Dynamic t) => Dynamic t (a, b, c) -> (Dynamic t a, Dynamic t b, Dynamic t c)
splitTriplePure d = (f1 <$> d, f2 <$> d, f3 <$> d)
  where
    f1 (x',_,_) = x'
    f2 (_,x',_) = x'
    f3 (_,_,x') = x'

  
data StructExample = StructExample { _a :: Int, _b :: Int } --deriving Show 
instance Show StructExample where
  -- shows like a JS object
  show examp = "{_a:" <> show (_a examp) <> ", _b:" <> show (_b examp) <> "}"  

testJSON :: StructExample
testJSON = StructExample 1 1 

faqEl'' :: (MonadFix m, DomBuilder t m) => Int -> T.Text -> m () -> m ()
faqEl'' n title body = mdo
  let imageId :: T.Text = "faq-image-" <> tshow n
  let borderId :: T.Text = "faq-border-" <> tshow n
  let containerId :: T.Text = "faq-" <> tshow n
  let openImg :: T.Text = $(staticImg "up.webp")
  let openBorder = $(classh' [ bc .~~ Black
                             --, h .~~ TWSize_Full
                             , bw_x .~~ B2, bw_t .~~ B2, br_t .~~ R_2Xl])
  let openContainer = $(classh' [ bw_x .~~ B2, bw_b .~~ B2, br_b .~~ R_2Xl, bgColor .~~ White, bc .~~ Black
                                , p .~~ TWSize 10
                                , custom .~ "z-50"
                                ])
  let closedImg :: T.Text = $(staticImg "down.webp")
  let closedBorder = $(classh' [ bc .~~ Black
                               --, h .~~ TWSize_Full
                               , bw .~~ B2, br .~~ R_2Xl ])
  let closedContainer :: T.Text = "hidden"
  let dropdownClass = $(classh' [custom .~ "relative", colSpan .~ zipScreens [2,2,1]])
  let JSFunc toggleFaq_ = js9 ("toggleFaq" :: String) containerId imageId borderId openImg openBorder openContainer closedImg closedBorder closedContainer

  _ <- elAttr "div" ("class" =: dropdownClass <> "onClick" =: (T.pack toggleFaq_)) $ do
    elClass' "div" $(classh' [ border . bColor . allS .~ only Black, border . radius . allS .~ only R_2Xl
                             , bgColor .~ only White, m .~ only (TWSize 4), custom .~ "drop-shadow-2xl"
                             ]) $ mdo
      elAttr "div" ("class" =: closedBorder <> "id" =: borderId) $ gridCol Col12 $ do
        elClass "div" $(classh' [colSpan .~ only 11, pos .~ centeredOnly, p .~ only (TWSize 4)]) $ do
          textS $(classh textStyleC [text_size .~ zipScreens [XL,XL2]]) title 
        el "div" $ do
          elAttr "img" ("src" =: $(staticImg "down.webp") <> "id" =: imageId) blank
      elAttr "div" ("class" =: closedContainer <> "id" =: containerId) $ do
        body
  pure ()


-- | This is opposite of FRP 
-- elTarget :: T.Text -> DomID -> Map AttributeName T.Text -> m a -> m (DomID, a)
-- elTarget tag id attrs ma = fmap (id, ) $ elAttr tag ("id" =: id <> attrs) ma 
  

faqEl' :: (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m ) => T.Text -> m () -> m ()
faqEl' title body = mdo
  isOpen <- holdDyn False $ leftmost 
    [ True <$ gate (current (not <$> isOpen)) (domEvent Click ddown)
    , False <$ gate (current isOpen) (domEvent Click ddown)
    , False <$ domEvent Mouseleave ddown
    ]
  let (imgSrc, border', ass) = splitTriplePure $ ffor isOpen dropdownView 
  (ddown,_) <- elClass "div" $(classh' [custom .~ "relative", colSpan .~ zipScreens [2,2,1]]) $ do
    elClass' "div" $(classh' [ border . bColor . allS .~ only Black, border . radius . allS .~ only R_2Xl
                             , bgColor .~ only White, m .~ only (TWSize 4), custom .~ "drop-shadow-2xl"
                             ]) $ mdo
      elDynClass "div" border' $ gridCol Col12 $ do
        elClass "div" $(classh' [colSpan .~ only 11, pos .~ centeredOnly, p .~ only (TWSize 4)]) $ do
          textS $(classh textStyleC [text_size .~ zipScreens [XL,XL2]]) title 
        el "div" $ do
          -- TODO: dynImage ; should have sizing and CSS controlled by shell
          elDynAttr "img" (("src" =:) <$> imgSrc) blank
      elDynClass "div" ass $ do
        body
  pure ()
