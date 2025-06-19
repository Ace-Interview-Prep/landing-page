{-# LANGUAGE OverloadedStrings #-}

module Landing.Pages.For.Recruiters where

import Landing.Pages.Elems
import Landing.Utils
import Classh
import Classh.Reflex
import Reflex.Dom.Core
import Control.Monad.Fix

landingRecruiters 
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , MonadHold t m
     )
  => m ()
landingRecruiters = do
  aceLandingHeader
  _ <- bannerFor $(staticImg "bannerImg_recruiters.webp") $ do
    row [b .~ only (TWSize 4)] $ 
      normalText "FOR TECH RECRUITERS"
    row [b .~ only (TWSize 4)] $ do      
      bannerTextLg "Unlock your talent's"
    row [b .~ only (TWSize 24)] $ 
      bannerTextBlue "full potential"
    bookDemoButton
    pure never
  secondRowSolutionPage
    $(staticImg "row2_feedback.webp")
    "Make sure your talents present exceptionally"
    "Equip your candidates with the essential communication skills training to win every interview."
    bookDemoButton
  testimonials
  coreValuePropForSolution
    "SAVE TIME"
    [ cvpText "Make your talent prep more", cvpTextBlue "effective and efficient." ]
    "Ace has all the features you need to make sure your candidates receive the training they need while streamlining your prep process."
  sol_4p_8t
    $(staticImg "recruiters_shareScores.webp")
    "Share candidate scores with clients"
    "With our PDF export feature, candidates can save their interview scores to share with clients via their candidate profile."
  sol_8t_4p
    $(staticImg "recruiters_unlimitedAccess.webp")
    "Offer candidates unlimited access to training"
    "We'll manage your candidates' ongoing practice, so you can focus on sourcing and placing top talent in their ideal roles."
  sol_4p_8t
    $(staticImg "recruiters_trackProgress.webp")
    "Track candidate progress with our admin dashboard"
    "Easily monitor candidate progress, from signup and onboarding to tracking performance history, all through Ace's admin view."
  statsSection
  callToAction
    "Ready to upskill your talent?"
    "Contact our team to chat about how we can help you prepare your candidates to excel in their upcoming interviews."
    bookDemoButton
  aceLandingFooter
  pure ()
