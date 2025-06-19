module Landing.Pages.Pricing where

import Landing.Utils
import Lamarckian.JS
import Landing.Pages.Elems
import Classh
import Classh.Reflex

import Reflex.Dom.Core

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Fix
import qualified Data.Text as T

pricingPage
  :: ( DomBuilder t m
     , MonadFix m
     , MonadIO m 
     )
  => m ()
pricingPage = do
  JSFunc clearDropdown <- aceTalentLandingHeader
  elAttr "div" ("onClick" =: T.pack clearDropdown) $ do
    void $ bannerFor $(staticImg "pricing_header_image.webp") $ do
      row [b .~ only (TWSize 4)] $ normalText "PRICING"
      row [b .~ only (TWSize 4)] $ do
        elClass "div" "max-w-[400px] md:max-w-[600px] lg:max-w-[800px]" $ do
          bannerTextMdBlue "Simple, transparent pricing"
          bannerTextMd " to fit exactly what your company is looking for."
      pure never
    row [b .~~ TWSize 16] $ do
      elClass "div" "mt-6 md:mt-6 mx-10 sm:mx-28 md:mx-40 border-t border-x border-b-[0.5px] border-[#00B9DA] rounded-t-xl" $ do
        elClass "div" "pt-12 text-center" $ do
          bannerTextMd "For Job Seekers:"
          bannerTextMdBlue " Free For Life!"
        responsiveXPaddedRegion' . elClass "div" "pt-4 pb-8 px-24 md:px-32 lg:px-48 text-center" $ do
          normalText "We are exceptionally passionate about what we do. While competitors charge as much as $2.5k per month for less services than we provide, we would rather see you put your best foot forward and succeed"
      elClass "div" "mb-0 md:mb-0 mx-10 sm:mx-28 md:mx-40 border-b border-x border-t-[0.5px] border-[#00B9DA] rounded-b-xl bg-[#F1FCFF]" $ do
        elClass "div" "my-12 grid grid-cols-12" $ do
          elClass "div" "col-start-3 col-span-8 mt-10 text-left" $ do
            normalText "THROUGH THIS PROGRAM YOU WILL BECOME:"
            el "ul" $ do
              elClass "li" "mt-4 flex items-start" $ do
                elClass "i" "fas fa-check-circle text-orange-500 text-2xl mr-2" $ blank
                el "div" $ do 
                  normalBlueText "A Senior Level Communicator "
                  normalText " who is able to lead and instruct intermediate and junior developers"
              elClass "li" "mt-4 flex items-start" $ do
                elClass "i" "fas fa-check-circle text-orange-500 text-2xl mr-2" $ blank
                el "div" $ do
                  
                  normalBlueText "A Senior Level Software Engineer,"
                  normalText " who is knowledgeable on the latest developments in engineering best practices and programming language theory"
              elClass "li" "mt-4 flex items-start" $ do
                elClass "i" "fas fa-check-circle text-orange-500 text-2xl mr-2" $ blank
                el "div" $ do
                  normalBlueText "Hired at a job you love"
                
    row [b .~~ TWSize 4] $ do
      elClass "div" "mt-0 md:mt-0 mx-10 sm:mx-28 md:mx-40 border-t border-x border-b-[0.5px] border-[#00B9DA] rounded-t-xl" $ do
        elClass "div" "pt-12 text-center" $ do
          bannerTextMd "One-Time, "
          bannerTextMdBlue "Success "
          bannerTextMd "Fee"
        elClass "div" "pt-4 pb-8 px-24 md:px-32 lg:px-48 text-center" $ do
          normalText "Only pay for success. Benefit from our services and advisory, with the guarantee that payment is made only when you secure your ideal candidate."
      elClass "div" "mb-20 md:mb-32 mx-10 sm:mx-28 md:mx-40 border-b border-x border-t-[0.5px] border-[#00B9DA] rounded-b-xl bg-[#F1FCFF]" $ do
        elClass "div" "my-12 grid grid-cols-12" $ do
          elClass "div" "col-start-3 col-span-2 text-left" $ do
            row [b .~ only (TWSize 4)] $ do
              boldTitleSm "JUNIOR"
            row [b .~ only (TWSize 4)] $ do
              boldBlueTitleSm "$5,000"
          elClass "div" "col-start-5 col-span-4 text-center" $ do
            row [b .~ only (TWSize 4)] $ do
              boldTitleSm "INTERMEDIATE"
            row [b .~ only (TWSize 4)] $ do
              boldBlueTitleSm "$7,500"
          elClass "div" "col-start-9 col-span-2 text-right" $ do
            row [b .~ only (TWSize 4)] $ do
              boldTitleSm "SENIOR"
            row [b .~ only (TWSize 4)] $ do
              boldBlueTitleSm "$10,000"

          elClass "div" "col-start-3 col-span-8 mt-10 text-left" $ do
            normalText "WITH ALL ROLES, YOU BENEFIT FROM:"
            el "ul" $ do
              elClass "li" "mt-4 flex items-start" $ do
                elClass "i" "fas fa-check-circle text-orange-500 text-2xl mr-2" $ blank
                normalText "Access to full candidate profile of soft, technical, and unique characteristics analysis"
              elClass "li" "mt-4 flex items-start" $ do
                elClass "i" "fas fa-check-circle text-orange-500 text-2xl mr-2" $ blank
                normalText "Advisory on interviewing, onboarding, and integration processes"
              elClass "li" "mt-4 flex items-start" $ do
                elClass "i" "fas fa-check-circle text-orange-500 text-2xl mr-2" $ blank
                normalText "90 day replacement guarantee, at no extra cost"
              elClass "li" "mt-6 italic flex items-start" $ do
                normalText "Note: All prices are in USD. Startup and bulk hiring packages available upon consultation."


    row [y .~~ TWSize 16] $ do
       elClass "div" "mb-12" $ do
         responsiveXPaddedRegion' $ callToActionBubble
           "https://meetings.hubspot.com/lauren974"
           "Book a call with us today to learn about our next hiring cohort."
           "Book a call"

    aceTalentLandingFooter
    pure ()
