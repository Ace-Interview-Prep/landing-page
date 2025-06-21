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


aceTalentBanner :: DomBuilder t m => m ()
aceTalentBanner = do
  elClass "div" $(classh' [px .|~ [(TWSize 20),(TWSize 28)], py .|~ [(TWSize 0),(TWSize 36)], bgColor .~~ aceLightBlue ]) $ do
    gridCol Col12 $ do
      col [12,12,8,8] $ do 
        row [y .~~ TWSize 10, t .~~ TWSize 20] $ do
          row [] $ do
            bannerTextBlue' "Exceptional "
            bannerTextLg' "tech talent, "
          row [] $ do
            bannerTextBlue' "Efficient "
            bannerTextLg' "hiring"
        row [r .|~ [(TWSize 0),(TWSize 24)], b .~~ TWSize 16] $ do
          normalText "Hiring with Ace Talent means access to the best talent the world has to offer, backed by a partner who truly cares about your business and your people."

        row [b .~~ TWSize 16] $ gridCol Col3 $ do
          col [3] $ elClass "div" "flex items-start mb-2" $ do
            elClass "i" "fas fa-check-circle text-orange-500 text-2xl mr-2" $ blank
            normalTextSm "Unique, upskilled talent"
          col [3] $ elClass "div" "flex items-start mb-2" $ do
            elClass "i" "fas fa-check-circle text-orange-500 text-2xl mr-2" $ blank
            normalTextSm "Sourced from top organizations"
          col [3] $ elClass "div" "flex items-start mb-2" $ do
            elClass "i" "fas fa-check-circle text-orange-500 text-2xl mr-2" $ blank
            normalTextSm "Remote & relocation ready"

        row [b .|~ [(TWSize 10),(TWSize 8)]] $ do
          Common.Elems.simpleLink "https://meetings.hubspot.com/lauren974" $ void hireWithAceButton
        row [b .|~ [(TWSize 20),(TWSize 8)]] $ do
          Common.Elems.simpleLink "https://meetings.hubspot.com/lauren974" $ void goJobSeekersButton
      col [12,12,4,4] $ elClass "div" "hidden md:block" $ do
        row [ y .~~ TWSize 12] $ do
          elClass "div" "grid grid-cols-12 gap-4" $ do
          --gridCol Col12 $ do
            colFrom [(0,6),(3,5)] $ row [b .~~ TWSize 16] $ do
              featuredProfile Black $(staticImg "Vanessa-Azenwa.webp") "Vanessa Azenwa" "Software Developer"
              featuredProfile Black $(staticImg "Joseph-Agboro.webp") "Joseph Agboro" "Technical Lead"
            col [6, 5] $ row [t .~~ TWSize 16] $ do
              featuredProfile Black $(staticImg "Kelvin-Ndoma.webp") "Kelvin Ndoma" "SEO + Software Developer"
              featuredProfile Black $(staticImg "Omonon-Stephens.webp") "Omonon Stephens" "Product Manager"

checkmark :: DomBuilder t m => T.Text -> m () 
checkmark txt = do
  imgClass $(staticImg "checkmark.webp") "inline-block" 
  elClass "div" "" $ normalText txt

featuredProfile :: DomBuilder t m => Color -> T.Text -> T.Text -> T.Text -> m () 
featuredProfile __borderColor imgSrc name profession = do
  elClass "div" "pb-20" $ do
    imgClass imgSrc $(classh' [ w .|~ [twSize' 36, twSize' 36, twSize' 36, twSize' 64]
                              , br .~~ R_3Xl
                              , bw .~~ B4
                              , bc .~~ Black
                              ])
    --row [] $ do
    row [t .~~ TWSize 2] $ textS $(classh' [text_weight .~~ Bold, text_size .~ normalTextSize]) name
    elClass "div" "" $ textS "" profession

-- | Does not follow template pattern
landingBase :: StaticWidget' r x ()
landingBase = mdo
  JSFunc clearDropdown <- aceTalentLandingHeader
  elAttr "div" ("onClick" =: T.pack clearDropdown) $ do
    aceTalentBanner
    howWeWork
    responsiveXPaddedRegion [pct 90, pct 80, pct 80, pct 80, pct 80, pct 70] $ sol_4p_4t'
      $(staticImg "step1.webp")
      "Step 1: Understand your business’ unique needs"
      [ row [b .~~ TWSize 2] $ normalText "We start by getting to know your business - your goals, challenges, and what defines success for the role beyond just the job description. "
      , row [] $ normalText "Taking this extra step helps us find a hire who’s not just qualified but genuinely aligned with your vision."
      ]
    responsiveXPaddedRegion [pct 90, pct 80, pct 80, pct 80, pct 80, pct 70] $ sol_4t_4p'
      $(staticImg "step2.webp")
      "Step 2: Curate your talent matches"
      [ row [b .~~ TWSize 2] $ normalText "Each profile we present highlights unique strengths and growth areas, showcasing essential soft skills, technical expertise, and notable qualities observed throughout their learning journey."
      , row [] $ normalText "This means you’ll get a clear, honest view of each candidate’s fit and growth potential."
      ]
    responsiveXPaddedRegion [pct 90, pct 80, pct 80, pct 80, pct 80, pct 70] $ sol_4p_4t'
      $(staticImg "step3.webp")
      "Step 3: Interview, Hire, Integrate"
      [ row [b .~~ TWSize 2] $ normalText "Once you’ve found the right match, we support you through the interview and hiring process. From there, our team provides hands-on guidance for onboarding, helping your new hire adapt to your culture while offering insights on collaborating across cultural differences."
      , row [] $ normalText "We’re with you all the way, setting your newest team member up for long-term success." 
      ]
    row [y .~~ TWSize 16] $ responsiveXPaddedRegion [pct 90, pct 80, pct 80, pct 80, pct 80, pct 70] $ callToActionBubble
      "https://meetings.hubspot.com/lauren974"
      "Skip the wasted time and cost from inefficient recruitment and focus on building your MVP team."
      "Hire With Ace Talent" 
    --responsiveXPaddedRegion' $ displayTestimonials' testimonialsData
    row [y .|~ [(TWSize 4), (TWSize 20), (TWSize 32)]] $ mdo
      skillsWeTrain
        commCopy
        fpCopy
        culturalCopy
    row [y .|~ [(TWSize 8),(TWSize 12),(TWSize 24)]] $ do
      responsiveXPaddedRegion [pct 90, pct 80, pct 80, pct 80, pct 80, pct 70] . responsiveXPaddedRegion' $ do
        divClass $(classh' [ pb .~~ TWSize 20, custom .~"text-center" ]) $ boldTitle "We source from the best"
        gridCol Col3 $ do
          let cont = elClass "div" $(classh' [pos .~ centeredOnly])
          let classhd = $(classh' [width' .~ zipScreens [pct 100, pct 80], px .~ only (TWSize 4)])
          let displayImage i = cont $ imgClass i classhd
          displayImage $(staticImg "alu.webp") 
          displayImage $(staticImg "moringa.webp") 
          displayImage $(staticImg "cmu.webp") 
  
    row [y .~~ TWSize 28] $ responsiveXPaddedRegion [pct 90, pct 80, pct 80, pct 80, pct 80, pct 70] $ callToActionBubble
      "https://meetings.hubspot.com/lauren974"
      "Ready to reserve your spot for the next hiring cohort?"
      "I'm ready!"
    row [ y .~~ TWSize 16 ] $ aceTalentLandingFooter


commCopy :: DomBuilder t m => [m ()]
commCopy =
  [ normalText "Through our Ace Interview Prep product, candidates engage in targeted exercises focused on essential skills such as storytelling and asking insightful questions. They participate in mock interviews and receive personalized, AI-driven feedback that evaluates 20+ soft skills and offering actionable insights. "
  , normalText "Every candidate from Ace Talent is not only technically skilled but also an exceptional collaborator and communicator."
  ] 

fpCopy :: DomBuilder t m => [m ()]
fpCopy = 
  [ normalText "We enhance our talents' technical leadership and excellence by training them in Haskell."
  , normalText "This approach sharpens their ability to think rigorously and build scalable, reliable code, equipping them to excel in"
    >> normalBolded " any tech stack "
    >> normalText "while bringing a unique problem-solving mindset to every role. Our projects are so challenging that even ChatGPT can't solve them!"
  ]

normalBolded :: DomBuilder t m => T.Text -> m ()
normalBolded = textS $(classh textStyleC [ text_size .~ normalTextSize
                                         , text_color .~~ aceNavyBlue
                                         , text_weight .~~ Bold 
                                         ])



culturalCopy :: DomBuilder t m => [m ()] 
culturalCopy = 
  [ normalText "Global teams thrive when cultural awareness is at the forefront. To ensure this, we've developed comprehensive training and resources, guided by experts, to prepare candidates for success in diverse teams and international environments. "
  , normalText "This training helps them navigate cultural nuances, adapt quickly, and collaborate effectively-allowing teams to work more cohesively and drive better results from day one."
  ]


type Highlighted = Bool

data Testimonial = Testimonial
  { _testimonial_name :: T.Text
  , _testimonial_jobTitle :: T.Text
  , _testimonial_orgName :: T.Text
  , _testimonial_headshot :: T.Text 
  , _testimonial_texts :: [(Highlighted, T.Text)]
  }

testimonialsData :: [Testimonial]
testimonialsData =
  [ Testimonial
    "Bright Kemasoude"
    "Alumni Engagement Lead"
    "African Leadership University"
    $(staticImg "Bright-Kemasoude.webp")
    [ (False, "Ace Talent’s commitment to upskilling and preparing candidates ensures that every individual is equipped with the")
    , (True, "necessary skills and insights to excel")
    , (False, "in their next role. Any company would be fortunate to partner with Ace and tap into their impressive talent pool, supported by a team that genuinely cares about fostering meaningful career connections.")
    ]
  , Testimonial
    "Magnifique Nsengimana"
    "Head of IT"
    "Innova8ive Tech Labs"
    $(staticImg "Magnifique-Nsengimana.webp")
    [ (False, "Working with Ace Talent has been an exceptional experience from start to finish. The process of finding and hiring a mobile developer was")
    , (True, "smooth, efficient, and highly professional.")
    , (False, "The talent pool provided was")
    , (True, "impressive, with candidates who were not only skilled but also a perfect cultural fit for our company.")
    , (False, "The level of support and communication throughout the process was outstanding, making it clear that Lauren and her team are dedicated to ensuring the success of both the candidates and the hiring companies. We’re thrilled to have a developer join our team and couldn’t be happier with the entire experience.")
    , (True, "I highly recommend Ace Talent’s services to any company looking to find top-tier talent.")
    ]
  ] 

displayTestimonials :: DomBuilder t m => [Testimonial] -> m ()
displayTestimonials tmonials = do
  elClass "div" $(classh' [bgColor .~ only aceLightBlue]) $ do
    row [y .~ only (TWSize 28)] . responsiveXPaddedRegion' . gridCol Col2 $ do
      forM_ tmonials $ responsiveShell . displayTestimonial
      
  where
    responsiveShell = col [2,2,1] . responsiveXPaddedRegion' 

displayTestimonials' :: DomBuilder t m => [Testimonial] -> m ()
displayTestimonials' tmonials = do
  elClass "div" $(classh' [bgColor .~ only aceLightBlue]) $ do
    row [y .|~ [(TWSize 10), (TWSize 28)]] . responsiveXPaddedRegion' . gridCol Col2 $ do
      forM_ tmonials $ responsiveShell . displayTestimonial'
      
  where
    responsiveShell = col [2,2,1] . responsiveXPaddedRegion' 


displayTestimonial :: DomBuilder t m => Testimonial -> m ()
displayTestimonial tmonial = do
  row [y .~ only (TWSize 20)] $ do 
    let quoteMarkFont = textS $(classh textStyleC [text_size .~~ XL9, text_weight .~~ Extrabold, text_custom .~ "leading-3"])
    elClass "div" $(classh' [h .|~ [pix 200, pix 200, pix 400, pix 400, pix 300], custom .~ "text-center"]) $
      quoteMarkFont "“" >> intercalate (normalTextSize) " " (choose <$> _testimonial_texts tmonial)
    divClass $(classh' [pt .~~ TWSize 10, position .~~ (J_Center, A_End)]) $ do 
      row [x .~ only (TWSize 0)] $ do
        gridCol Col3 $ do
          elClass "div" "" $ do
            imgClass (_testimonial_headshot tmonial) $(classh' [h .~~ TWSize_Full, w .~~ TWSize_Full, br .~~ R_Full])
          elClass "div" $(classh' [colSpan .~~ 2, pos .~~ (J_Start, A_Center), pl .~~ (TWSize 10)]) $ do
            let italic = textS $(classh textStyleC [text_custom .~ "italic", text_size .~ normalTextSize ])
            row [] $ textS $(classh textStyleC [text_weight .~~ Bold, text_size .~ normalTextSize]) $ _testimonial_name tmonial
            row [] $ italic $ _testimonial_jobTitle tmonial
            row [] $ italic $ _testimonial_orgName tmonial

  where
    choose :: DomBuilder t m => (Bool, T.Text) -> m ()
    choose (highlight, txt) = if highlight then normalBlueText txt else normalText txt



displayTestimonial' :: DomBuilder t m => Testimonial -> m ()
displayTestimonial' tmonial = do
  row [y .|~ [(TWSize 10), (TWSize 20)]] $ do 
    let quoteMarkFont = textS $(classh textStyleC [text_size .|~ [XL6, XL9], text_weight .~~ Extrabold, text_custom .~ "leading-3"])
    elClass "div" "text-center" $
      quoteMarkFont "“" >> intercalate (normalTextSize) " " (choose <$> _testimonial_texts tmonial)
    divClass $(classh' [pt .~~ TWSize 10, position .~~ (J_Center, A_End)]) $ do 
      row [x .~ only (TWSize 0)] $ do
        gridCol Col3 $ do
          elClass "div" "" $ do
            imgClass (_testimonial_headshot tmonial) $(classh' [h .~~ TWSize_Full, w .~~ TWSize_Full, br .~~ R_Full])
          elClass "div" $(classh' [colSpan .~~ 2, pos .~~ (J_Start, A_Center), pl .~~ (TWSize 10)]) $ do
            let italic = textS $(classh textStyleC [text_custom .~ "italic", text_size .~ normalTextSize ])
            row [] $ textS $(classh textStyleC [text_weight .~~ Bold, text_size .~ normalTextSize]) $ _testimonial_name tmonial
            row [] $ italic $ _testimonial_jobTitle tmonial
            row [] $ italic $ _testimonial_orgName tmonial

  where
    choose :: DomBuilder t m => (Bool, T.Text) -> m ()
    choose (highlight, txt) = if highlight then normalBlueText txt else normalText txt





howWeWork :: DomBuilder t m => m ()
howWeWork = responsiveXPaddedRegion' $ coreValuePropForSolution
  "HOW WE WORK"
  [ cvpText "Skills-based hiring done right" ]
  "Strong relationships lead to stronger outcomes. That’s why our entire process is around understanding your business to help you find the perfect hire."
  
landingBaseBanner :: (PostBuild t m, DomBuilder t m) => m (Event t ())
landingBaseBanner = do
  --el "div" $ do
  let c = $(classh' [
               --height'.~ zipScreens [pix 450, pix 200, pix 200, pix 265, pix 325, pix 450]
                       bgColor .~ only aceLightBlue
                      , custom .~ "relative h-[50vh] sm:h-[60vh] md:h-[70vh] lg:h-[80vh] w-[100%]"
                      , pt .~ only (TWSize 48)
                      ]
           )
  elClass "div" c $ do
    elClass "div" $(classh' [pos .~ centeredOnly, custom .~ "absolute z-50 w-full"]) $ do
      elClass "div" $(classh' [width' .~ zipScreens [pct 100, pct 50], pos .~ centeredOnly, custom .~ "text-center"]) $ do
        imgAttr $(staticImg "FUUUUUUUUUUUUUUUUUUUUUCK.svg") ("class" =: "md:w-full")
    clk <- elClass "div" $(classh' [width' .~ zipScreens [pct 100], pos .~ only (J_Center,A_End), custom .~ "absolute text-center z-50"]) $ do        
      elClass "div" $(classh' [w .~ zipScreens [pct 40], pos .~ centeredOnly, pt .~ zipScreens [TWSize 32,TWSize 32,TWSize 44,TWSize 56,TWSize 72,TWSize 96]]) $ do
        --row [y .~ zipScreens [TWSize 20, TWSize 10, TWSize 20]] $ do
          textS $(classh textStyleC [text_size .~ zipScreens [LG,XL, XL2,XL3,XL4,XL5], text_color .~ only aceNavyBlue])
            "Boost your confidence, master interviews, and turn your dream job into a reality."
          elClass "div" "pt-10 z-60" $ simpleLinkFE (FrontendRoute_Pages :/ RecordMock :/ FreeRecord :/ ()) tryAceButton

    -- left-28 top-1/2
    -- right-32 top-1/2
    --imgAttr $(staticImg "banner-laptop-girl.webp") ("class" =: $(classh' [custom .~ "absolute z-30", width' .~ only (pct 25) ]))
    elClass "div" $(classh' [pos .~ only (J_Start ,A_Start), custom .~ "absolute z-30 w-full", py .~ only (TWSize 32), px .~ only (TWSize 20)])  $ do
      imgAttr $(staticImg "banner-laptop-girl.svg") ("class" =: $(classh' [ width' .~ only (pct 25)]))

    elClass "div" $(classh' [pos .~ only (J_End ,A_End), custom .~ "absolute z-30 w-full", py .~ only (TWSize 32), px .~ only (TWSize 20)]) $ do
      imgAttr $(staticImg "banner-letter.svg") ("class" =: $(classh' [ width' .~ only (pct 20)]))
    pure clk

tailoredFeedbackSection :: DomBuilder t m => m (Event t ())
tailoredFeedbackSection = responsiveXPaddedRegion' $ do
  -- padding is cuz of chaos from banner
  elClass "div" $(classh' [pt .~+ zipScreens [pix 100, pix 150, pix 150, pix 200, pix 200, pix 200]] ) $ do 
    elClass "div" $(classh' [pos .~ only centered]) $ do
      row [b .~ only (TWSize 10)] $ do
        normalText "TAILORED FEEDBACK"
      elClass "div" $(classh' [pb .~ only (TWSize 20), custom .~ "text-center"]) $ do 
        let
          size7 = boldTitle
          size7blue = boldBlueTitle
        intercalate (zipScreens [XL2, XL3, XL5, XL6, XL7]) " " [ size7 "Know exactly", size7blue "what", size7 "and", size7blue "how", size7 "to improve" ]
      --let txt = textS $(classh textStyleC [text_size .~ zipScreens [XL ,XL3]])
      elClass "div" $(classh' [pb .~ only (TWSize 24), pos .~ centeredOnly]) $ do
        elClass "div" $(classh' [w .~ zipScreens [pct 50], custom .~ "text-center"]) $ do 
          normalText "With tailored feedback, you can make sure you've got the right answers and that your delivery was spot on "
      clk <- row [b .~ only (TWSize 24)] $ simpleLinkFE (FrontendRoute_Pages :/ RecordMock :/ FreeRecord :/ ()) tryAceButton
      row [b .~ zipScreens [TWSize 40, TWSize 40, TWSize 40, TWSize 96], x .~ zipScreens [pix 0, TWSize 24]] $ imgAttr $(staticImg "feedbackCategories_landing.webp") mempty
      pure clk

supportedBySection :: DomBuilder t m => m ()
supportedBySection = do
  elClass "div" $(classh' [bgColor .~ only aceLightBlue, width' .~ only TWSize_Full, py .~ zipScreens [TWSize 10, TWSize 40]] ) $ do
    elClass "div" $(classh' [pb .~ only (TWSize 10), pos .~ centeredOnly]) $ do
      --textS $(classh textStyleC [text_size .~ zipScreens [XL5,XL5,XL7], text_weight .~ only Semibold]) "Supported by"
      boldTitle "Supported by"
    responsiveXPaddedRegion' $ do
      let imgClassSupp = $(classh' [width' .~ zipScreens [pct 100, pct 80], px .~ only (TWSize 4)]) 
      let cont = elClass "div" $(classh' [pos .~ centeredOnly])
      gridCol Col5 $ do
        cont $ imgAttr $(staticImg "EDGE_Sheridan.webp") ("class" =: $(classh' [width' .~ zipScreens [pct 80, pct 60], px .~ only (TWSize 4)]))
        cont $ imgAttr $(staticImg "league_of_innovators_logo.webp") ("class" =: imgClassSupp)
        cont $ imgAttr $(staticImg "NEXT_Logo_2019.webp") ("class" =: imgClassSupp)
        cont $ imgAttr $(staticImg "RBC_FL_logo.webp") ("class" =: imgClassSupp)
        cont $ imgAttr $(staticImg "idea-sauga-logo.webp") ("class" =: imgClassSupp)


curatedPrepSection :: DomBuilder t m => m (Event t ()) 
curatedPrepSection = do
  row [y .~ zipScreens [TWSize 20, TWSize 40, TWSize 40, TWSize 60]] $ do
    responsiveXPaddedRegion' . gridCol Col10 $ do
      tryAce <- elClass "div" $(classh' [colSpan .~ zipScreens [10, 10, 5, 5], custom .~ "text-center md:text-left"]) $ do        
        row [] $ do
          normalText "CURATED, INTERACTIVE PREP"
        row [t .~ only (TWSize 8)] $ do
          paragraphs (only (TWSize 6))
            [ boldTitle "Show up " >> boldBlueTitle "well-prepared"
            , boldTitle "and " >> boldBlueTitle "ready to wow"
            , normalText "Not sure how to prepare for your interview? Our exercises have got you covered, from storytelling skills to asking questions that make you more memorable"
            ]
        row [y .~ only (TWSize 36)] $ simpleLinkFE (FrontendRoute_Pages :/ RecordMock :/ FreeRecord :/ ()) tryAceButton 
      elClass "div" $(classh' [colSpan .~ zipScreens [10,10, 5, 5], pos .~ only (J_End, A_Center), custom .~ "inline-block"]) $  do 
        imgAttr $(staticImg "exercise_dir_landing.webp") ("class" =: $(classh' [width' .~ only (pct 100)]))
      pure tryAce 

reviewsSection :: (DomBuilder t m) => m () 
reviewsSection = do 
  _ <- elClass "div" "relative" $ do
    (e,_) <- elClass' "div" $(classh' [width' .~ only TWSize_Full, height' .~ only TWSize_Full, custom .~ "absolute z-30"]) $ do 
      --let bolded = textStyle & text_size .~ XL6 & text_weight .~ Extrabold
      let bolded = textS $(classh textStyleC [text_size .~ zipScreens [XL2, XL3, XL5, XL7], text_weight .~ only Extrabold])
      let boldedBlue = textS $(classh textStyleC [text_size .~ zipScreens [XL2, XL3, XL5, XL7], text_weight .~ only Extrabold, text_color .~ only aceBlue'])
      responsiveXPaddedRegion' $ do
        row [y .~ zipScreens [TWSize 4, TWSize 4, TWSize 10, TWSize 16, TWSize 28]] $ paragraphs mempty $
          [ intercalate titleSize " " [ bolded "Hear what", boldedBlue "our users", bolded "have"]
          , bolded "to say..."
          ]
        elAttr "div" ("class" =: $(classh' [height' .~ only (pct 100)]) <> "id" =: "scrollableContainer") $ do
          let cc = $(classh' [py .~ only (TWSize 4), height' .~ [("def",pct 80),("lg",pct 65),("xl", pct 60), ("2xl", pct 85)], custom .~ "flex overflow-x-auto hover-scrollbar"])  
          elAttr "div" ("class" =: cc <> "id" =: "scrollableReviews") $ do
            customerReviews
              [ ("Yes, the questions were very helpful. I loved the fact that they were \
                 \ random behavioural questions which would enable me think fast for a logical answer I could give."
                ,"Kosisochukwu Oparaku"
                ,"UX/UI Designer"
                )
              , ("I've learned a lot from using the Ace tool to practice behavioral interview questions. \
                 \The customized feedback reports are excellent and give detailed instructions on how to \
                 \improve. My ability to communicate clearly and concisely has grown, as well as my confidence. I advise every job searcher to use Ace."
                , "Paul Chinedu"
                , "Backend Developer"
                )
              , ("I was blown away by the user experience and personalized feedback provided by this \
                 \mock-interview platform. It helped me identify my strengths and weaknesses, and gave \
                 \me the confidence I needed to approach my real interview. Highly recommend for anyone looking to \
                 \nail their next job interview!"
                ,"Hector Munachi"
                ,"Frontend Engineer"
                )
              , ("I luckily used Ace and I'm glad that I did. It has helped me to polish my conversational skills, \
                 \and I have seen a drastic improvement in my confidence. The feedback report was extremely \
                 \helpful as well and gave me insights into what I'm doing right & wrong."
                ,"Fahad Shoaib"
                ,"Software Engineer"
                )
              , ("The Ace platform has been impactful and helped me improve my interviewing skills."
                ,"Abdulrahman Yusuf"
                ,"Backend Developer"
                )
              ] 
    userReviewBackdrop
    pure e
  pure ()
  -- prerender_  blank $ do
  --   isFocused <- holdDyn False $ leftmost [ True <$ domEvent Mouseenter focusable, False <$ domEvent Mouseleave focusable ] 
  --   pb <- delay 10 =<< getPostBuild 
  --   t <- countTimeFrom 1 pb
  --   let t' = ffilter (\t_ -> round t_ `mod` 5 == 0) t
  --   performEvent $ ffor (gate (not <$> current isFocused) t') $ \_ -> liftJSM $ do
      
  --     scrollable <- jsg (s "document") ^. js1 (s "getElementById") (s "scrollableReviews")
  --     elWidth :: Maybe Float <- fromJSVal =<< jsg (s "document") ^. js1 (s "getElementById") (s "scrollableContainer") ^. js (s "scrollWidth")
  --     --elWidth2 :: Maybe Float <- fromJSVal =<< (GHCJS.unElement . _element_raw $ focusable) ^. js "scrollWidth"
  --     clog scrollable
  --     --elWidth :: Maybe Float <- fromJSVal =<< scrollable ^. js (s "scrollWidth")
  --     case elWidth of
  --       Nothing -> clogSend (s "failed to scroll")
  --       Just w -> scrollElem scrollable (w / 3, 0)
      
  --   pure ()


--callJS :: forall a. ToJSON a => String -> [a] -> String

userReviewBackdrop :: DomBuilder t m => m () 
userReviewBackdrop = do
  let blueFilter = $(classh' [ width' .~ only TWSize_Full
                             , height' .~ only TWSize_Full
                             , bgColor .~ only aceLightBlue2
                             , bgOpacity .~ only 80                             
                             , custom .~ "absolute z-20" ]) 
  elClass "div" blueFilter blank
  elClass "div" "z-10" $ do 
    let
      imgAs = "class" =: $(classh' [p .~ only (TWSize 0)]) --"col-span-1 py-6 px-6" -- <&> w   
    gridCol Col7 $ do
      imgAttr $(staticImg "diverse_1_1.webp") ("class" =: $(classh' [p .~ only (TWSize 0), colStart .~ only 4]))
      imgAttr $(staticImg "diverse_1_2.webp") imgAs
      imgAttr $(staticImg "diverse_1_3.webp") imgAs
      imgAttr $(staticImg "diverse_2_3.webp") imgAs
    gridCol Col7 $ do
      imgAttr $(staticImg "diverse_2_1.webp") imgAs
      imgAttr $(staticImg "diverse_2_2.webp") imgAs
      imgAttr $(staticImg "diverse_2_3.webp") imgAs
      imgAttr $(staticImg "diverse_2_4.webp") imgAs
      imgAttr $(staticImg "diverse_2_5.webp") imgAs
      imgAttr $(staticImg "diverse_2_6.webp") imgAs
      imgAttr $(staticImg "diverse_2_7.webp") imgAs     
    gridCol Col7 $ do 
      imgAttr $(staticImg "diverse_3_1.webp") imgAs
      imgAttr $(staticImg "diverse_3_2.webp") imgAs
      imgAttr $(staticImg "diverse_3_3.webp") imgAs
      imgAttr $(staticImg "diverse_3_4.webp") imgAs
      imgAttr $(staticImg "diverse_3_5.webp") imgAs
      imgAttr $(staticImg "diverse_3_6.webp") imgAs
      imgAttr $(staticImg "diverse_3_7.webp") imgAs
    elClass "div" "sm:hidden" $ gridCol Col7 $ do
      imgAttr $(staticImg "diverse_2_1.webp") imgAs
      imgAttr $(staticImg "diverse_2_2.webp") imgAs
      imgAttr $(staticImg "diverse_2_3.webp") imgAs
      imgAttr $(staticImg "diverse_2_4.webp") imgAs
      imgAttr $(staticImg "diverse_2_5.webp") imgAs
      imgAttr $(staticImg "diverse_2_6.webp") imgAs
      imgAttr $(staticImg "diverse_2_7.webp") imgAs     

featuresSection :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => m (Event t ())
featuresSection = mdo
  chosen <- responsiveXPaddedRegion' $ elClass "div" $(classh' [pos .~ centeredOnly,pt .~ only (TWSize 60)]) $ do
    normalText "GET READY THE ACE WAY"
    elClass "div" $(classh' [pos .~ centeredOnly, pt .~ only (TWSize 8)]) $ do
      --gridCol Col3 $ do 
      gridColW Col3 (pct 100) $ do
        newOpt <- elClass "div" "hidden" $ fmap leftmost $ forM [Prepare, Practice, Progress] $ \opt -> featureSelectButton chosen opt
        holdDyn Prepare newOpt
  elClass "div" $(classh' [pb .~ only (TWSize 48), px .~ only (TWSize 10), pt .~ only (TWSize 20), height' .~ only (pix 200)]) $ do
    let bolded = textS $(classh textStyleC [text_size .~ normalTextSize, text_weight .~ only Bold, custom .~ "italic"])      
    dyn_ $ ffor chosen $ \case
      Prepare -> 
        featureDisplay' $(staticImg "prepare_feature.webp") $ intercalate normalTextSize " "
          [ normalText "Navigating generic guides, blogs, and how-to’s for your interview prep can be frustrating. We get it. That’s why we offer"
          , bolded "curated, fun exercises"
          , normalText "that give you genuine ways to prepare for tricky topics."
          ]
      Practice -> do
        featureDisplay' $(staticImg "practice_feature.webp") $ intercalate normalTextSize " " 
          [ normalText "Our blend of randomized"
          , bolded "general behavioural, role-specific, and brainteaser"
          , normalText "questions will challenge you, refining your interviewing skills and keeping you sharp."
          ]
      Progress -> do
        featureDisplay' $(staticImg "progress_feature.webp") $ intercalate normalTextSize "" 
          [ normalText "Ace gives you personalized feedback on "
          , bolded "20+ soft skills"
          , normalText ", guiding you to know exactly what to improve for your next interview."
          ]
    elClass "div" $(classh' [pos .~ centeredOnly, py .~ only (TWSize 10)])  $  simpleLinkFE (FrontendRoute_Pages :/ RecordMock :/ FreeRecord :/ ()) tryAceButton

rolesSupportedSection :: DomBuilder t m => m () 
rolesSupportedSection = do
  elClass "div" $(classh' [pos .~ centeredOnly, pt .~ zipScreens [pix 200, pix 300, pix 400, pix 450, pix 500, pix 700], custom .~ "text-center"]) $ do  
    boldTitle "ROLES WE CURRENTLY SUPPORT"
  responsiveXPaddedRegion' . responsiveXPaddedRegion' . elClass "div" $(classh' [pt .~ only (TWSize 20)]) $ (gridColWhen (zipScreens [Col2, Col2, Col3])) $ do
    forM_ (Map.elems streamDisplayMap) $ \role -> supportedRole role
  elClass "div" $(classh' [pt .~ only (TWSize 24), pos .~ centeredOnly]) $ do
    elAttr "a" ("class" =: "" <> "style" =: "text-decoration: none;" <> "href" =: "https://calendly.com/aceinterviewprep/meet-with-lauren") $ do
      normalBlueText "Don't see your role? Request it here"

customerReview :: DomBuilder t m => Bool -> (T.Text, T.Text, T.Text) -> m () 
customerReview mrBool (msg, who, role) = do
  let marginRight = if mrBool then "mr-12" else ""
  let base = $(classh' [ bgColor .~ only White
                       , border . radius . allS .~ only R_2Xl
                       , bgOpacity .~ only 100
                       , px .~ only (TWSize 8)
                       , pt .~ only (TWSize 8)
                       , height' .~ only (TWFraction 7 D12)
                       , width' .~ zipScreens [pct 78, pct 48, pct 48, pct 32]
                       , custom .~ "flex-none inline-block leading-[0px]"
                       ]
              )
  elClass "div" (base <&> marginRight) $ do 
    let txt = textS $(classh textStyleC [text_size .~ zipScreens [SM, SM, Base, XL, XL2, XL3]])
    let txtBold = textS $(classh textStyleC [text_weight .~ only Bold, text_size .~ zipScreens [SM, SM, Base, XL, XL2, XL3]])
    elClass "div" $(classh' [height' .~ zipScreens [pct 50, pct 75], custom .~ "overflow-y-scroll hover-scrollbar"]) $ do 
      txt $ "“" <> msg <> "”"
    row [] $ do
      txtBold who
    row [] $ do
      txt role

customerReviews :: DomBuilder t m => [(T.Text, T.Text, T.Text)] -> m ()
customerReviews [] = pure () -- only possible if total == 0
customerReviews (review:[]) = customerReview False review 
customerReviews (review:rs) = customerReview True review >> customerReviews rs  

featureSelectButton :: (PostBuild t m, DomBuilder t m) => Dynamic t Feature -> Feature -> m (Event t Feature) 
featureSelectButton dynChosen thisOpt = do
  let
    txtBlue = $(classh textStyleC [text_weight .~ only Bold, text_color .~ only aceBlue', text_size .~ zipScreens [LG,XL3]])
    txtWhite = $(classh textStyleC [text_weight .~ only Bold, text_color .~ only White, text_size .~ zipScreens [LG,XL3]])
    baseClass = $(classh' [ pos .~ centeredOnly
                          , py .~ zipScreens [TWSize 2, TWSize 6], px .~ zipScreens [TWSize 4,TWSize 8], mx .~ only (TWSize 6)                            
                          , border .~ (def & bColor . allS .~ only aceBlue' & radius . allS .~ only R_3Xl & bWidth . allS .~ only B2)
                          ])                 
    className = ffor ((== thisOpt) <$> dynChosen) $ \case
      True -> "bg-[#00B9DA]" <&> baseClass
      False -> baseClass
    textClass = ffor ((== thisOpt) <$> dynChosen) $ \case
      True -> txtWhite 
      False -> txtBlue 
  (e, _) <- elDynClass' "button" className $ do
    textDynS textClass $ tshow thisOpt
  pure $ thisOpt <$ domEvent Click e 
  
data Feature = Prepare | Practice | Progress deriving (Eq, Show)

featureDisplay' :: DomBuilder t m => T.Text -> m () -> m () 
featureDisplay' imgSrc description = do
  gridCol Col12 $ do
    elClass "div" $(classh' [colStart .~ only 3, colSpan .~ only 10]) $ do 
      gridColW Col12 (pct 90) $ do
        elClass "div" $(classh' [pos .~ only (J_End,A_Center), height' .~ only TWSize_Full, w .~ [("2xl", pct 80)], colSpan .~ only 5]) $ do
          el "div"  description
        elClass "div" $(classh' [colSpan .~ only 7, pos .~ centeredOnly]) $ do
          imgAttr imgSrc  ("class" =: $(classh' [h .~ zipScreensWith vh [20,30,40,50,60,70]]) ) -- 

dropdownView :: Bool -> (T.Text, T.Text, T.Text)
dropdownView = \case
  True -> ( $(staticImg "up.webp")
          , $(classh' [ bc .~~ Black, h .~~ TWSize_Full, bw_x .~~ B2, bw_t .~~ B2, br_t .~~ R_2Xl  ])
          , $(classh' [ bw_x .~~ B2, bw_b .~~ B2, br_b .~~ R_2Xl, bgColor .~~ White, bc .~~ Black
                      , p .~~ TWSize 10
                      , custom .~ "z-50"
                      ])
          )
  False -> ( $(staticImg "down.webp")
           , $(classh' [ bc .~~ Black, h .~~ TWSize_Full, bw .~~ B2, br .~~ R_2Xl ])
           , "hidden"
           )

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


supportedRole :: DomBuilder t m => T.Text -> m ()  
supportedRole role = do 
  elClass "div" $(classh' [pos .~ centeredOnly, pt .~ zipScreens [TWSize 0, TWSize 0, TWSize 8]]) $ do
    gridCol Col12 $ do
      elClass "div" $(classh' [colStart .~ only 2, pt .~ only (TWSize 4), pos .~ centeredOnly]) $ imgAttr $(staticImg "checkmark_role.webp") ("class" =: "")
      elClass "div" $(classh' [colSpan .~ only 9, pos .~ only (J_Start, A_End), pl .~ only (TWSize 10)]) $ do
        textS $(classh textStyleC [text_size .~ zipScreens [Base, LG, XL, XL2, XL4]]) role 

