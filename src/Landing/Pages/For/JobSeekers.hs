{-# LANGUAGE OverloadedStrings #-}

module Landing.Pages.For.JobSeekers where

import Landing.Pages.Elems
import Lamarckian.JS
import Lamarckian.Types
import Landing.Utils
import Classh
import Classh.Reflex
import Common.Elems (simpleLink)
import Common.Route

import Templates.Partials.Image
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad (void)
import qualified Data.Text as T

-- upskill as an engineer and get hired in a community designed for your success

landingJobSeekers :: StaticWidget' r x ()
landingJobSeekers = do
  JSFunc clearDropdown <- aceTalentLandingHeader
  elAttr "div" ("onClick" =: T.pack clearDropdown) $ do 
    jobSeekersBanner

    row [y .~~ TWSize 42] $ gridCol Col12 $ elClass "div" $(classh' [colStart .~~ 2, colSpan .~~ 10]) $ gridCol Col12 $ do
      elClass "div" $(classh' [colSpan .~ zipScreens [12], pos .~~ centered, custom .~ "block md:hidden px-20 pb-12"]) $ do
        imgAttr $(staticImg "hiring-trends.webp") ("class" =: $(classh' [w .~~ TWSize_Full]))

      elClass "div" (classhUnsafe [colSpan .~ zipScreens [12, 12, 8], pos .~ only (J_Start, A_Center), px .~ only (TWSize 10)]) $ do
        el "div" . responsiveXPaddedRegion' $ do
          row [b .~~ TWSize 10] $ normalTextSm "LAND YOUR NEXT ROLE"
          row [b .~~ TWSize 10] $ bannerTextSm "Join a community designed for your success"
          row [b .~~ TWSize 10] $ normalTextSm "Ace Talent Community is your go-to place to learn, upskill, and land your next job. Through polishing your skills and direct recruitment, feel confident landing your next role with us."
          row [] $ Common.Elems.simpleLink "https://share.hsforms.com/1rzTZxmpSQSiM-cuwE92I8Qsmgty" $ void tryAceButton

      elClass "div" $(classh' [colSpan .~ zipScreens [3], pos .~~ centered, custom .~ "hidden md:block"]) $ do
        imgAttr $(staticImg "hiring-trends.webp") ("class" =: $(classh' [w .~~ TWSize_Full]))

    buildingSkillset

    row [b .|~ [(TWSize 16), (TWSize 32)]] $ skillsWeTrain
      commCopy
      fpCopy
      culturalCopy

    howWeWork

    row [y .~~ TWSize 20] $ do
      responsiveXPaddedRegion' $ sol_4t_4p''
        $(staticImg "apprentice-2.webp")
        "Step 1: Join the community and start upskilling right away"
       [ row [b .~~ TWSize 2] $ normalText "Our community is entirely self-led. When you join, we’ll learn more about what you’re looking for in your next role. Then, it’s up to you to utilize our learning materials to make the most of the Ace Talent Community resources."
       , row [] $ normalText "The more you learn and engage, the more perks available for you, from job match priority to direct coaching calls with our Mentors."
       ]
      responsiveXPaddedRegion' $ sol_4p_4t''
        $(staticImg "specialist-2.webp")
        "Step 2: Work with Expert Haskell mentors"
        [ row [b .~~ TWSize 2] $ normalText "New to functional programming? Not to worry! Anyone can learn Haskell, but you’ll gain more than just a new language. "
        , row [] $ normalText "Work with our Technical Leads to develop a “thinker” mindset, learn how to ask unique and thoughtful questions, and cultivate leadership skills within your role."
        ]
      responsiveXPaddedRegion' $ sol_4t_4p''
        $(staticImg "master-2.webp")
        "Step 3: Find your next match and get support along the way"
        [ row [b .~~ TWSize 2] $ normalText "Once you’re ready, we connect you with exciting job opportunities that align with your skills and aspirations. We post these within the community platform, then vet talent from there to ensure strong alignment with the company’s mission, culture, and role requirements."
        , row [] $ normalText "Our dedicated team supports you throughout the application and interview process, ensuring you are well-prepared and confident every step of the way."
        ]


    row [b .~~ TWSize 36, t .|~ [(TWSize 18), (TWSize 36)]] $ responsiveXPaddedRegion [pct 90, pct 80, pct 80, pct 80, pct 80, pct 70] $ do
      callToActionBubble
        "https://share.hsforms.com/1rzTZxmpSQSiM-cuwE92I8Qsmgty"
        "Ready to land your dream job?"
        "Sign Up Now"
    aceTalentLandingFooter

jobSeekersBanner :: DomBuilder t m => m () 
jobSeekersBanner = do
  el "div" $ do
    elClass "div" $(classh' [ custom .~ "relative", bgColor .~ only (hex "F1FCFF"), h .~ zipScreens [vh 30, vh 40, vh 80]]) $ do
      elClass "div" $(classh' [ custom .~ "absolute z-30", w .~ only TWSize_Full, h .~ only TWSize_Full, pos .~ centeredOnly]) $ do
        elClass "div" "text-center" $ do
          --row [] $ cvpText "FOR TALENT"
          row [b .~~ TWSize 4] $ normalText "FOR TALENT"
          row [t .~~ TWSize 8] $ do
            boldTitle "Your "
            boldBlueTitle "dream job "
            boldTitle "awaits"
          row [b .~~ TWSize 12, t .~~ TWSize 4] . responsiveXPaddedRegion' $ normalText "Upskill as an engineer and get hired in a community designed for your success"
          gridCol Col2 $ do
            divClass $(classh' [pos .~~ (J_End,A_Center), pr .~~ TWSize 4]) $
              simpleLinkFE (FrontendRoute_Login :/ ()) $ void loginButton
            divClass $(classh' [pos .~~ (J_Start,A_Center), pl .~~ TWSize 4]) $
              simpleLink "https://share.hsforms.com/1rzTZxmpSQSiM-cuwE92I8Qsmgty" $ void tryAceButton
      elClass "div" $(classh' [ custom .~ "z-20 absolute", w .~ only TWSize_Full, h .~ only TWSize_Full, pos .~ only (def, A_End)]) $ do
        elAttr "img" ("src" =: $(staticImg "bannerImg_bootcamps.webp") <> "class" =: $(classh' [w .~ only (TWFraction 1 D3)])) blank


buildingSkillset :: DomBuilder t m => m ()
buildingSkillset = coreValuePropForSolution'
  "BUILDING YOUR SKILL SET FOR SUCCESS"
  [ cvpText "Polish the skills that matter most" ]
  "By enhancing essential soft and technical skills that address common gaps in the market, we know exactly the skills you need to stand out in an increasingly competitive job market."

howWeWork :: DomBuilder t m => m ()
howWeWork = coreValuePropForSolution'
  "HOW IT WORKS"
  [ cvpText "You are 3 steps away from your next dream job" ]
  "Strong relationships lead to stronger outcomes. That’s why our entire process is around understanding your business to help you find the perfect hire."

commCopy :: DomBuilder t m => [m ()]
commCopy = 
    [ normalText "Through Ace Interview Prep, you'll dive into targeted exercises to strengthen your communication skills, including storytelling and asking impactful questions. You'll participate in mock interviews and receive personalized, AI-driven feedback that evaluates over 20 soft skills, providing actionable insights to enhance your communication, confidence, and clarity."
    , normalText "Ace Talent equips you to be not just technically skilled, but a standout communicator and effective team player."
    ]
fpCopy :: DomBuilder t m => [m ()]
fpCopy = 
    [ normalText "We build your technical leadership and problem-solving skills through Haskell training - no prior experience needed! Learning Haskell sharpens your ability to think critically and create scalable, reliable code, preparing you to excel in any tech stack with a unique, rigorous approach to solving complex challenges. "
    , normalText "You'll tackle projects so advanced that even AI can't solve them, setting you apart as a true technical leader."
    ]
culturalCopy :: DomBuilder t m => [m ()]
culturalCopy = 
    [ normalText "We know that cultural awareness is key to thriving on global teams. That's why we offer in-depth training and resources led by experts to prepare you for success in diverse, international environments. "
    , normalText "This training helps you navigate cultural nuances, adapt seamlessly, and collaborate effectively - so you can contribute confidently and help your team achieve great results from day one."
    ]
