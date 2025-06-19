module Landing.Pages.FAQ where

import Classh
import Classh.Reflex
import Landing.Pages.Elems
import Landing.Utils
import Lamarckian.JS
import Common.Constants

import Reflex.Dom.Core

import Control.Monad (void)
import Control.Monad.Fix
import Control.Monad.IO.Class
import qualified Data.Text as T

faq
  :: ( MonadFix m
     , DomBuilder t m
     , MonadIO m     
     )
  => m ()
faq = do
  JSFunc closeDropdown <- aceTalentLandingHeader
  
  elAttr "div" ("onClick" =: T.pack closeDropdown) $ do
    void $ bannerFor $(staticImg "bannerImg_communities.webp") $ do
      row [b .~ only (TWSize 4)] $ normalText "FAQ"
      row [b .~ only (TWSize 4)] $ do
        elClass "div" "max-w-[400px] md:max-w-[600px] lg:max-w-[800px]" $ do
          bannerTextMdBlue "Got questions? "
          bannerTextMd "We've got answers."
      pure never

    row [y .~~ TWSize 32] faqSection
    row [y .~~ TWSize 28] $ responsiveXPaddedRegion' $ callToActionBubble
      "https://meetings.hubspot.com/lauren974"
      "Have more questions or want to start getting talent profiles?"
      "Book a call"

    aceTalentLandingFooter
  
faqSection
  :: ( MonadFix m
     , DomBuilder t m
     )
  => m () 
faqSection = do
  elClass "div" $(classh' [bgColor .~ only aceLightBlue, py .~ only (TWSize 24), mt .~ only (TWSize 24)]) $ responsiveXPaddedRegion' . gridCol Col6 $ do  
    elClass "div" $(classh' [colSpan .~ zipScreens [6,6,6,1], pos .~ centeredOnly, custom .~ "text-center"]) $ do
      boldTitle "Frequently Asked Questions"

    elClass "div" $(classh' [colSpan .~ zipScreens [6,6,6,5]]) $ responsiveXPaddedRegion' $ do
      let txts ts = paragraphs (only $ TWSize 4) $ fmap normalText ts
      gridCol Col2 $ do
        faqEl'' 0 "Why should I choose Ace Talent over other recruitment partners?" $ txts
          ["At Ace Talent, our focus is clear: we prioritize building strong relationships with each company we work with and connecting you to exceptional talent. This involves a deep understanding of what great talent looks like, managing a top-tier service that puts your business first, and emphasizing quality over quantity. We don’t cut corners; our commitment is to find the perfect match for both you and our candidates, ensuring a successful hiring experience."
          ]
        faqEl'' 1 "Do you really only charge on successful hire?" $ txts
          [ "Yes, really. We recognize that large upfront fees can create uncertainty, especially without a guarantee of a successful hire. That’s why we maintain transparency, offer competitive pricing below traditional recruitment rates, and focus on delivering exceptional service every step of the way. Your peace of mind is our priority."
          ]
      -- row [t .~ only (TWSize 10)] $ do 
      --   gridCol Col2 $ do
        faqEl'' 2 "Why do candidates need extra upskilling?" $ txts
          [ "The tech industry is always evolving, and we've identified three key areas where skill gaps are impacting today's and tomorrow's roles: communication, cultural adaptability, and specialized technical expertise. That’s why we source exceptional talent from our exclusive network and equip them with the essential polishing skills they need to stand out and thrive in the ever-changing tech landscape."
          ]
        faqEl'' 3 "What if a hire doesn’t work out?" $ txts
          [ "We offer a full refund within the first 30 days if a hire isn’t successful, along with a free replacement within the first 90 days. We’re confident that with a thorough understanding of your needs throughout the search process, we will identify the perfect talent for your team."
          ]
      --row [t .~ only (TWSize 10)] $ do 
        --gridCol Col2 $ do
        faqEl'' 4 "Why wouldn’t I just source talent on my own?" $ txts
          ["An inefficient recruitment process can lead to $75,000 a month in wasted time, effort, and money."
          , "Partnering with Ace Talent allows you to leverage our expertise, extensive network, and tailored processes to identify and evaluate candidates effectively. We also provide in-depth insights into candidate strengths and growth areas, making the hiring process more efficient and effective."
          ]
        faqEl'' 5 "How do you ensure candidates are culturally adaptable?" $ txts
          ["We believe training for cultural adaptability is of the utmost importance - both for the talent to feel confident and prepared for their new environment, and to encourage more productive teamwork. Global teams outperform the rest, so we make sure your team is set up to perform properly."
          ]
          -- let para t = row [b .~ only (TWSize 4)] $ normalText t
          -- para "Reach out to us any day, any time. We’re always here to help you in your interview prep journey. "
          -- row [b .~ only (TWSize 4)] $ do
          --   normalText "You can reach our support team via "
          --   elAttr "a" ("href" =: "mailto:lauren@aceinterviewprep.io") $ do
          --     normalBlueText "email"
          --   normalText " or "
          --   elAttr "a" ("class" =: "" <> "style" =: "text-decoration: none;" <> "href" =: "https://help.aceinterviewprep.io/en/") $ do
          --     normalBlueText "live chat! "
            --routeLink (Landing :/ LandingBase :/ ()) $ 
  
  

faqEl'' :: (MonadFix m, DomBuilder t m) => Int -> T.Text -> m () -> m ()
faqEl'' n title body = mdo
  let imageId = "faq-image-" <> tshow n
  let borderId = "faq-border-" <> tshow n
  let containerId = "faq-" <> tshow n
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
  let closedContainer :: T.Text = "hidden" ---- $(classh' [ bc .~~ Black, h .~~ TWSize_Full, bw .~~ B2, br .~~ R_2Xl ])
  let dropdownClass = $(classh' [custom .~ "relative", colSpan .~ zipScreens [2,2,1]])
  let JSFunc toggleFaq_ = js9 "toggleFaq" containerId imageId borderId openImg openBorder openContainer closedImg closedBorder closedContainer

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
