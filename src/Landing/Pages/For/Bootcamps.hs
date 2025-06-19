{-# LANGUAGE OverloadedStrings #-}

module Landing.Pages.For.Bootcamps where 

import Landing.Utils
import Landing.Pages.Elems
import Classh
import Classh.Reflex

import Reflex.Dom.Core

import Control.Monad.Fix

landingBootcamps
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , MonadHold t m
     )
  => m ()
landingBootcamps = do
  aceLandingHeader
  _ <- bannerFor $(staticImg "bannerImg_bootcamps.webp") $ do
    row [b .~ only (TWSize 4)] $ normalText "FOR CODING BOOTCAMPS"
    row [b .~ only (TWSize 4)] $ bannerTextLg "Empower your students with"
    row [b .~ only (TWSize 24)] $ bannerTextBlue "personalized training"    
    bookDemoButton
    pure never
  secondRowSolutionPage
    $(staticImg "row2_feedback.webp")
    "Equip your students with job-ready skills"
    "While you focus on building exceptional technical skills, we'll prepare your students for successful interviews with essential soft skills practice."
    bookDemoButton
  testimonials
  coreValuePropForSolution
    "OFFER TRAINING AT SCALE"
    [ cvpTextBlue "Enhance" , cvpText "your career services offering" ]
    "Optimize your career services team by providing personalized, 1-to-1 support as you grow your student base." 
  sol_8t_4p
    $(staticImg "bootcamps_trackStudentProgress.webp")
    "Track student progress with our admin dashboard"
    "Easily monitor student progress and help them review their performance, all through Ace's admin view."
  sol_4p_8t
    $(staticImg "bootcamps_offerStudents.webp")
    "Offer students unlimited access to training"
    "Access to exercises, mock interviews, and rapport practice is unlimited for students throughout their program, ensuring ongoing training and progress."
  statsSection
  callToAction
    "Ready to supercharge your community?"
    "Learn more about how Ace is the perfect complement to your school's offerings and career services"
    bookDemoButton 
  aceLandingFooter
  pure ()
  pure () 
  pure ()
