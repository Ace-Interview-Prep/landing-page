{-# LANGUAGE OverloadedStrings #-}

module Landing.Pages.AboutUs where

import Landing.Pages.Elems
import Lamarckian.JS
import Landing.Utils
import Common.Constants
import Classh
import Classh.Reflex
import Templates.Partials.Image

import Reflex.Dom.Core

import Control.Monad.IO.Class
import Control.Monad.Fix
import qualified Data.Text as T

aboutUs
  :: ( DomBuilder t m
     , MonadFix m
     , MonadIO m  
     )
  => m () 
aboutUs = do
  JSFunc closeDropdown <- aceTalentLandingHeader
  elAttr "div" ("onClick" =: T.pack closeDropdown) $ do 
    elClass "div" $(classh' [pos .~ centeredOnly, py .~ only (TWSize 40), custom .~ "text-center"]) $ do
      row [] $ textS $(classh textStyleC [text_size .~ only XL9, text_weight .~ only Bold, text_color .~ only aceBlue']) "Our Story"
      row [t .~ only (TWSize 10)] $ do
        responsiveXPaddedRegion' . elClass "div" "text-center" $ do
          normalText "Founded out of frustration, we became committed to helping qualified individuals secure their dream jobs."
    responsiveXPaddedRegion' $ elClass "div" $(classh' [pos.~centeredOnly,py.~only(TWSize 10)]) $ do
       imgAttr $(staticImg "aceJourney_vertical.webp") ("class" =: "w-auto h-auto md:w-0 md:h-0")
       imgAttr $(staticImg "aceJourney.svg") ("class" =: "w-0 h-0 md:w-auto md:h-auto") 
    row [b .~ only (TWSize 40)] $ responsiveXPaddedRegion' . responsiveXPaddedRegion' . responsiveXPaddedRegion' . elClass "div" "text-center" $ do
      elAttr "a" ("href" =: "https://meetings.hubspot.com/lauren974") $ textS'' "Want to learn more about our entrepreneurial journey? "
      normalText "We love connecting with fellow entrepreneurs to share our experiences and insights about getting from 0 to 1, developing an AI product, and all things building."
    elClass "div" "text-center" $ bannerTextLg "Meet the team"
    row [b .~ only (TWSize 0), t .~ only (TWSize 40)] $ responsiveXPaddedRegion' . responsiveXPaddedRegion' $ do
      elClass "div" $(classh' [h .~ zipScreensWith vh [30,40,50,60,70], custom .~ "relative"]) $ do
        imgAttr $(staticImg "meet_the_team_backdrop.webp") ("class" =: $(classh' [w .~ only TWSize_Full, h .~ only TWSize_Full, custom .~ "z-0 absolute"]))
        elClass "div" "absolute z-20 w-full h-full" $ elClass "div" "h-full grid grid-cols-2" $ do 
          teamMember $(staticImg "lauren_headshot.webp") "Lauren DeSouza" "Co-founder and CEO" "https://www.linkedin.com/in/desouzalauren/"
          teamMember $(staticImg "galen_headshot.webp") "Galen Sprout" "Co-founder and CTO" "https://www.linkedin.com/in/galen-sprout-lazylambda/"
    row [b .~ only (TWSize 40), t .~ zipScreens [TWSize 20, TWSize 36, TWSize 48, TWSize 56,pix 350]] $ responsiveXPaddedRegion' . responsiveXPaddedRegion' . elClass "div" "text-center" $ do
      elAttr "a" ("href" =: "https://meetings.hubspot.com/lauren974") $ textS'' "Want to get in touch? "
      normalText "Whether it’s about your interviewing process or to learn more about Ace, we want to hear from you."
    aceTalentLandingFooter 

textS'' :: DomBuilder t m => T.Text -> m ()
textS'' = textS $(classh textStyleC [ text_color .~ only aceBlue'
                                    , text_weight .~ only Semibold
                                    , text_decoration .~ (def & textDec_line .~ only Underline & textDec_color .~ only aceBlue')
                                    , text_size .~ normalTextSize 
                                    ])

teamMember :: DomBuilder t m => T.Text -> T.Text -> T.Text -> T.Text -> m ()
teamMember imgPath name role linkedIn = do
  elClass "div" $(classh' [pos .~ centeredOnly, px .~ only (TWSize 8)]) $ el "div" $ do
    elClass "div" $(classh' [pos .~ centeredOnly]) $ imgAttr imgPath ("class" =: $(classh' [pt .~ only (TWSize 0), w .~ only (pct 60)]))
    elClass "div" $(classh' [pos .~ centeredOnly, custom .~ "text-center"]) $ do
      row [y .~ only (TWSize 4)] $ textS $(classh textStyleC [text_weight .~ only Bold, text_size .~ normalTextSize]) name
      row [b .~ only (TWSize 4)] $ normalText role
      row [] $ elAttr "a" ("href" =: linkedIn) $ textS $(classh textStyleC [text_size .~ normalTextSize, text_decoration . textDec_line .~ only Underline]) "Connect on LinkedIn"

