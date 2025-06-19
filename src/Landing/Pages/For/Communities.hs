{-# LANGUAGE OverloadedStrings #-}

module Landing.Pages.For.Communities where 

import Landing.Utils
import Landing.Pages.Elems
import Classh
import Classh.Reflex

import Reflex.Dom.Core

import Control.Monad.Fix

landingCommunities
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , MonadHold t m
     )
  => m ()
landingCommunities = do
  aceLandingHeader
  _ <- bannerFor $(staticImg "bannerImg_communities.webp") $ do
    row [b .~ only (TWSize 4)] $ normalText "FOR TECH COMMUNITIES"
    row [b .~ only (TWSize 4)] $ do
      bannerTextBlue "Supercharge"
      bannerTextLg " your"     
    row [b .~ only (TWSize 24)] $ bannerTextLg " community's offerings"
    bookDemoButton
    pure never
  secondRowSolutionPage
    $(staticImg "row2_feedback.webp")
    "Give your members a unique advantage"
    "Offer your members the platform that will help them win every interview."
    bookDemoButton
  testimonials
  coreValuePropForSolution
    "ENHANCE YOUR OFFERING"
    [ cvpText "Make your offering more", cvpTextBlue "unique and valuable." ]
    "Partnering with Ace means you get to offer a unique, one-of-a-kind service to your members."
  sol_8t_4p
    $(staticImg "communities_accessEasy.webp")
    "Access to easy, effortless practice"
    "Give your members the ability to practice in a judgement-free zone at their own pace with a wide range of tools to grow their communication skills\
    \ at their own pace."
  sol_4p_8t
    $(staticImg "communities_exercises.webp")
    "Exercises that prepare job seekers like a pro"
    "With exclusive exercises covering tough topics like How to Approach Questions and The Ins & Outs of \
    \Interviewing, your members have everything they need to shine."
  sol_8t_4p
    $(staticImg "communities_empower.webp")
    "Empower your members with the ultimate resource"
    "Boost your community’s offerings with access to unlimited mock interviews, evergreen content, and invaluable training."
  statsSection
  callToAction
    "Ready to supercharge your community?"
    "Contact our team to discuss how Ace is the perfect complement to your community’s offerings."
    bookDemoButton
  aceLandingFooter
  pure ()
