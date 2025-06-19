{-# LANGUAGE OverloadedStrings #-}

module Landing.Pages.Elems where

import Landing.Utils
import Lamarckian.JS
import Lamarckian.Types
import qualified Common.Elems
import Common.Constants
import Templates.Partials.Image
import Classh as C
import Classh.Reflex as C
import Common.Route

import Obelisk.Route.Frontend
import Landing.Static
import Reflex.Dom.Core

import Control.Monad (void, forM_)
import Control.Monad.IO.Class
import Control.Monad.Fix
import qualified Data.Map as Map 
import qualified Data.Text as T

-- | idea: just carries name to function made for notifying system of event
-- buttonStaticFRP :: T.Text -> JSRef
-- button



type Src = T.Text



-- Fixes
  -- Images
         
  -- Links
  -- Buttons
  -- Text
   -- size must be larger than 12px (@ 6% , need 60%) 





-- TODO: make Classh setter .|*> :: (a -> b) -> [a] -> SetsB
aceTalentLandingHeader
  :: ( DomBuilder t m
     , MonadFix m
     , MonadIO m
     )
  => m JSFunc
aceTalentLandingHeader = mdo
  row [x .|~ [(TWSize 4),(TWSize 10)], y .|~ [(TWSize 2), (TWSize 0)]] $ do
    -- Desktop Header
    elClass "div" "flex justify-between items-center" $ do
      elClass "div" "flex items-center" $ simpleLinkBE (Landing :/ LandingBase :/ mempty) $ imgClass $(staticImg "ace-talent-logo-textual-blue.webp") $(classh' [p .|~ [TWSize 2, TWSize 8], h .|~ (twSize' <$> [16,28,36,40])])
      elClass "div" "hidden sm:flex sm:space-x-8 md:space-x-8 lg:space-x-12" $ do
        elAttr "div" ("onClick" =: T.pack setRolesEl <> "class" =: "") $ centerSimple $ blueNavbarText "Roles"
        elAttr "div" ("onClick" =: T.pack setBlogEl <> "class" =: "cursor-pointer") $ centerSimple $ blueNavbarText "Learn"
        simpleLinkBE (Landing :/ Pricing :/ ()) $ centerSimple $ blueNavbarText "Pricing"
        simpleLinkBE (Landing :/ FAQ :/ ()) $ centerSimple $ blueNavbarText "FAQs"
        simpleLinkBE (Landing :/ AboutUs :/ ()) $ centerSimple $ blueNavbarText "About Us"
        simpleLinkBE (Landing :/ ForTalent :/ ()) $ centerSimple $ void $ blueBoldButton "For Talent"
        Common.Elems.simpleLink "https://meetings.hubspot.com/lauren974" $ centerSimple $ void hireWithAceButton
      elClass "div" "flex sm:hidden" $ do
        centerHSimple $ elClass "div" "focus:outline-none md:hidden" $ do
          elAttr "button" ( "onClick" =: "toggleOpenDropdown()"
                          <> "aria-label" =: "mobile navigation menu"
                          <> "class" =: "p-2 rounded-lg shadow-lg bg-white text-blue-500 hover:bg-blue-100 focus:outline-none") $ do
            elClass "i" "fas fa-bars text-2xl text-[#00B9DA]" blank

    -- Mobile Dropdown Menu
    let alignR = divClass $(classh' [pos .~~ (J_Start,A_Center), w .~~ TWSize_Full, pr .~~ TWSize 8])
    elAttr "div" ("id" =: "solutionsDropdown" <> "class" =: "hidden my-4 pl-2 text-right space-y-4") $ do
      elClass "div" "mt-4" $ elAttr "div" ("onClick" =: T.pack setRolesEl <> "class" =: "") $ alignR $ blueNavbarText "Roles"
      elClass "div" "mt-4" $ elAttr "div" ("onClick" =: T.pack setBlogEl <> "class" =: "cursor-pointer") $ alignR $ blueNavbarText "Learn"
      elClass "div" "mt-4" $ simpleLinkBE (Landing :/ Pricing :/ ()) $ alignR $ blueNavbarText "Pricing"
      elClass "div" "mt-4" $ simpleLinkBE (Landing :/ FAQ :/ ()) $ alignR $ blueNavbarText "FAQs"
      elClass "div" "mt-4" $ simpleLinkBE (Landing :/ AboutUs :/ ()) $ alignR $ blueNavbarText "About Us"
      elClass "div" "mt-4" $ simpleLinkBE (Landing :/ ForTalent :/ ()) $ alignR $ void $ blueBoldButton "For Talent"
      elClass "div" "mt-6" $ Common.Elems.simpleLink "https://meetings.hubspot.com/lauren974" $ alignR $ void hireWithAceButton

  elAttr "div" ("id" =: "dd") blank
  (JSFunc setRolesEl, JSFunc setBlogEl, JSFunc clear) <- fmap has3 $ staticDynWithToggle "dd"
    [ rolesWePlace
    , blogHighlights
    , blank
    ]
  pure $ JSFunc clear


-- Note: this should only ever be used in a static dom builder Template Haskell context 
has3 :: [JSFunc] -> (JSFunc,JSFunc,JSFunc)
has3 (x_:y_:z_:_) = (x_,y_,z_)
has3 _ = error "incorrect number of dom setters"

aceTalentLandingFooter :: DomBuilder t m => m ()
aceTalentLandingFooter = do
  gridCol Col12 $ do
    col [6] $ divClass $(classh' [w .~~ TWSize_Full, h .~~ TWSize_Full ]) $ do
      elClass "div" "pl-12 max-w-80" $ do
        imgClass $(staticImg "ace-talent-logo-textual-blue.webp") ""
      elClass "div" "pl-12 pt-6 max-w-80 md:max-w-96" $ do
        normalTextLg "Your partner in finding exceptional tech talent."

    col [6] $ elClass "div" "pl-6" $ do
      row [b .~~ TWSize 2] $ textS $(classh' [text_weight .~~ Medium, text_size .~ titleSizeSm]) "COMPANY"
      simpleLinkBE (Landing :/ AboutUs :/ ()) $ row [b .~~ TWSize 2] $ normalText "About us"
      simpleLinkBE (Landing :/ Pricing :/ ()) $ row [b .~~ TWSize 2] $ normalText "Pricing"
      simpleLinkBE (Landing :/ FAQ :/ ()) $ row [b .~~ TWSize 2] $ normalText "FAQs"
      Common.Elems.simpleLink "https://www.linkedin.com/company/acetheinterview" $ row [b .~~ TWSize 2] $ normalText "LinkedIn"

    col [12] $ elClass "div" "pl-12 py-12" $ do
      row [b .~~ TWSize 2] $ textS $(classh' [text_weight .~~ Medium, text_size .~ titleSizeSm]) "ROLES"
      gridCol Col2 $ do
        forM_ (Map.elems streamDisplayMap) (row [b .~~ TWSize 4] . normalText )


blueBoldButton :: DomBuilder t m => T.Text -> m (Event t ())
blueBoldButton txt = do
  (e, _) <- elClass' "button" "bg-[#00B9DA] py-3 sm:py-4 px-2 md:px-8 rounded focus:outline-none transition duration-300 ease-in-out hover:scale-105 drop-shadow-2xl" $ do
    textS $(classh textStyleC [ text_color .~ only White
                              , text_size .~ zipScreens [Base, LG,XL,XL2,XL3]
                              , text_weight .~ only Bold 
                              ] 
           ) txt
  pure $ domEvent Click e 


hireWithAceButton :: DomBuilder t m => m (Event t ())
hireWithAceButton = blueBoldButton "Hire With Ace Talent"

goJobSeekersButton :: DomBuilder t m => m (Event t ())
goJobSeekersButton = blueBoldButton "Join and Upskill for free as a Talent"
  
-- -- | Move to Reflex.Classh as a util
-- centerSimple :: DomBuilder t m => m a -> m a 
-- centerSimple = elClass "div" $(classh' [ position .~~ centered, w .~~ pct 100, h .~~ pct 100 ])

-- -- | Move to Reflex.Classh as a util
-- centerHSimple :: DomBuilder t m => m a -> m a 
-- centerHSimple = elClass "div" $(classh' [ position .~~ centered, w .~~ pct 100 ])

-- -- | Move to Reflex.Classh as a util
-- centerVSimple :: DomBuilder t m => m a -> m a 
-- centerVSimple = elClass "div" $(classh' [ position .~~ centered, h .~~ pct 100 ])


-- -- | A dumb shell which does nothing but visually wrap a DomBuilder
-- -- | is for documentation purposes
type DomShell m a = m a -> m a
-- responsiveXPaddedRegion' :: DomBuilder t m => DomShell m a 
-- responsiveXPaddedRegion' = centerHSimple . elClass "div" $(classh' [w .|~ [pct 100, pct 80, pct 80, pct 80, pct 80, pct 70]])

-- -- TODO: Kyle edits: Review/remove this
-- responsiveXPaddedRegion'' :: DomBuilder t m => DomShell m a
-- responsiveXPaddedRegion'' = centerHSimple . elClass "div" $(classh' [w .|~ [pct 90, pct 80, pct 80, pct 80, pct 80, pct 70]])

headerDropdownContainer :: DomBuilder t m => DomShell m a 
headerDropdownContainer ma = do
  divClass $(classh' [bgColor .~~ aceLightBlue, mb .~~ TWSize 20]) $ do
    divClass $(classh' [br .~~ R_3Xl, bc .~~ aceBlue', bw .~~ B2, mx .~~ TWSize 8, my .~~ TWSize 4, p .~~ TWSize 8]) $ do
      ma
      
rolesWePlace :: DomBuilder t m => m ()
rolesWePlace = headerDropdownContainer $ do
  row [] $ boldTitle "ROLES WE PLACE"
  row [] $ normalText "Don't see a role you're looking for? Let us know - we're always keeping an eye out for great talent!"
  row [] $ divClass $(classh' [pos .~~ centered]) $ do
    gridCol Col12 $ do
      col [4] $ do
        divClass $(classh' [pos .~~ centered, h .~~ TWSize_Full]) $ imgClass $(staticImg "roles-fam.webp") $(classh' [w .~~ vw 30])
      col [8] $ row [allS .~~ TWSize 20] $ gridColWhen (zipScreens [Col1, Col2, Col3]) $ do
        -- forM_ (Map.elems streamDisplayMap) $ \role -> supportedRole role
        forM_ (Map.elems streamDisplayMap) 
          (row [b .|~ (TWSize <$> [4,4,4,20]), x .~~ TWSize 4] . textS $(classh textStyleC [text_size .|~ [LG, LG, LG, XL4]]))
  pure ()

skillsWeTrain
  :: [StaticWidget' r x ()]
  -> [StaticWidget' r x ()]
  -> [StaticWidget' r x ()]
  -> StaticWidget' r x () 
skillsWeTrain commCopy fpCopy culturalCopy = mdo 
  row [t .~~ TWSize 16] $ responsiveXPaddedRegion [pct 90, pct 80, pct 80, pct 80, pct 80, pct 70] $ elClass "div" "grid grid-cols-3 gap-4 md:gap-10 lg:gap-20" $ do--gridCol Col3 $ do
    skillOptionSelect
      "Communication Skills"
      (JSFunc setCommSkills)
      $(staticImg "comm-bubbles.webp")
    skillOptionSelect
      "Functional Programming"
      (JSFunc setFPSkills)
      $(staticImg "haskell-logo-orange.webp")
    skillOptionSelect
      "Cultural Adaptability"
      (JSFunc setAdapt)
      $(staticImg "orange-globe.webp")
  row [y .~~ TWSize 16] $ responsiveXPaddedRegion' $ do
    elAttr "div" ("id" =: "d") $ communicationSkills commCopy 
  (JSFunc setCommSkills,JSFunc setFPSkills,JSFunc setAdapt) <- fmap has3 $ staticDynamicDOM "d" --[]
    [ communicationSkills commCopy
    , functionalProgrammingSkills fpCopy
    , culturalAdaptabilitySkills culturalCopy
    ]
  pure ()

blogHighlights :: DomBuilder t m => m ()
blogHighlights = headerDropdownContainer $ do
  row [] $ boldTitle "LEARN WITH US"
  row [] $ normalText "Dive deeper into our upskilling methodology, market trends, and the things that are top of mind for hiring exceptional talent."
  row [y .~~ TWSize 16] $ divClass $(classh' [box_custom .~ "overflow-auto w-[100%]"]) $ do
    gridCol Col3 $ do      
      col [3,1] $ blogCard $(staticImg "hiring-trends.webp") "BLOG" "2025 Tech Hiring Trends to Look Out For" "https://medium.com/all-things-ace/2025-tech-hiring-trends-to-look-out-for-33c72b9d93f5"
      col [3,1] $ blogCard $(staticImg "vanessa-case-study.webp") "CASE STUDY" "Why Now Is the Time to Invest in African Tech Talent" "https://medium.com/all-things-ace/why-now-is-the-time-to-invest-in-african-tech-talent-040a55360093"
      col [3,1] $ blogCard $(staticImg "psychology-of-skills-based-hiring.webp") "BLOG" "A New Era in Talent Acquisition: Skills-Based Hiring" "https://medium.com/all-things-ace/a-new-era-in-talent-acquisition-skills-based-hiring-00f2ed827bd1"

blogCard :: DomBuilder t m => ImgSrc -> T.Text -> T.Text -> T.Text -> m ()
blogCard imgSrc blogType description linkTo = do
  divClass $(classh' [br .~~ R_3Xl, bc .~~ aceBlue', bw .~~ B2, mx .~~ TWSize 8, my .~~ TWSize 4]) $ Common.Elems.simpleLink linkTo $ do
    centerHSimple $ imgClass imgSrc "p-4 h-24 md:h-64"
    divClass $(classh' [bc .~~ aceBlue', bw_t .~~ B2, px .~~ TWSize 12, py .~~ TWSize 4]) $ do
      row [b .~~ TWSize 2] $ textS $(classh' [text_weight .~~ Bold, text_size .~ normalTextSize]) blogType
      row [] $ normalText description


skillOptionSelect :: DomBuilder t m => T.Text -> JSFunc -> ImgSrc -> m ()
skillOptionSelect title jsf imgSrc = do
  divClass $(classh' [px .~~ TWSize 0, pos .~~ centered]) $ do
    primaryButtonStatic' jsf (always aceBlue') $ gridCol Col12 $ do
      col [12,12,4] $ centerHSimple $ do
        imgClass imgSrc $(classh' [h .~~ twSize' 12, custom .~ "pt-2"]) -- "h-12 inline-block"
      col [12,12,8] $ do
        let words_ = fmap T.pack $ words $ T.unpack title
        forM_ words_ $ row [] . normalWhiteText 
        --normalWhiteText title
  where
    normalWhiteText = textS $(classh textStyleC [text_size .~ normalTextSize, text_color .~~ White ])

communicationSkills :: [StaticWidget' r x ()] -> StaticWidget' r x ()
communicationSkills copy = do 
  skillCard
    "Communication Skills Training"
    copy 
    $(staticImg "communication-skills.webp")

functionalProgrammingSkills :: DomBuilder t m => [m ()] -> m ()
functionalProgrammingSkills copy = do
  skillCard
    "Functional Programming Course"
    copy 
    $(staticImg "functional-programming-card.webp")
  -- where
    
  --   normalBolded = textS $(classh textStyleC [ text_size .~ normalTextSize
  --                                            , text_color .~~ aceNavyBlue
  --                                            , text_weight .~~ Bold 
  --                                            ])
 
culturalAdaptabilitySkills :: (t ~ SpiderTimeline Global, DomBuilder t m) => [m ()] -> m ()
culturalAdaptabilitySkills copy = do
  skillCard
    "Cultural Adaptability"
    copy
    $(staticImg "cultural-adaptability.webp") 


-- TODO: Need a better way to switch ordering for certain sizes
-- this is fine for now
skillCard :: DomBuilder t m => T.Text -> [m ()] -> ImgSrc -> m ()
skillCard title paras imgSrc = do 
  skillCardShell $ gridCol Col12 $ do
    col [12] $ elClass "div" "block md:hidden" $
      centerVSimple $ imgClass imgSrc ""
    col [12] $ elClass "div" "block md:hidden" $ do
      row [y .~~ TWSize 8] $ cvpTextBlue title 
      row [] $ do
        forM_ paras $ row [b .~~ TWSize 12]

    
    col [7] $ elClass "div" "hidden md:block" $ do
      row [y .~~ TWSize 8] $ cvpTextBlue title
      row [] $ do
        forM_ paras $ row [b .~~ TWSize 12]
    col [5] $ elClass "div" "hidden md:block" $
      centerVSimple $ imgClass imgSrc ""

skillCardShell :: DomBuilder t m => m a -> m a
skillCardShell = divClass $(classh' [bgColor .~~ aceLightBlue, p .~~ TWSize 20, br .~~ R_3Xl]) 

callToActionBubble :: DomBuilder t m => T.Text -> T.Text -> T.Text -> m () 
callToActionBubble url pitch action = do
  elClass "div" $(classh' [br .~~ R_3Xl, bgColor .~~ aceBlue', pb .|~ [(TWSize 10), (TWSize 0)]]) $ do 
    gridCol Col12 $ do
      col [12, 8] $ divClass $(classh' [py .|~ [(TWSize 10), (TWSize 40)], px .~~ TWSize 10]) $ do
        textS $(classh' [text_size .~ titleSizeMd, text_color .~~ White]) pitch
      col [12, 4] $ Common.Elems.simpleLink url $ centerSimple $ void $ primaryButtonC [("hover",hex "FF823C"), ("def",hex "FF823C")] action
  pure ()


-- | Primary button with configurable colors 
primaryButtonStatic :: DomBuilder t m => JSFunc -> WhenTW Color -> T.Text -> m ()
primaryButtonStatic (JSFunc jsf) cfg buttonText = do
  elAttr "button" ("class" =: classhd <> "name" =: name <> "onClick" =: T.pack jsf) $ do
    textS $(classh' [text_size .~ normalTextSize]) buttonText
  pure ()
  where
    classhd = classhUnsafe [ bgColor .~ cfg
                           , custom .~ (primaryClass_ <&> "py-4 px-8")
                           ]
    -- for testing / selenium mainly
    name = "" -- T.filter (\c -> isAlphaNum c || isSpace c ) buttonText

-- | Primary button with configurable colors
-- | the prime denotes here that we have more control over the inner
-- | TODO: write non-primes in terms of prime 
primaryButtonStatic' :: DomBuilder t m => JSFunc -> WhenTW Color -> m () -> m ()
primaryButtonStatic' (JSFunc jsf) cfg inner = do
  elAttr "button" ("class" =: classhd <> "name" =: name <> "onClick" =: T.pack jsf) $ do
    inner
  pure ()
  where
    classhd = classhUnsafe [ bgColor .~ cfg
                           , width' .~~ pct 100
                           , custom .~ (primaryClass_ <&> "py-4 px-2")
                           ]
    -- for testing / selenium mainly
    name = "" -- T.filter (\c -> isAlphaNum c || isSpace c ) buttonText



-- | Primary button with configurable colors 
primaryButtonC :: DomBuilder t m => WhenTW Color -> T.Text -> m (Event t ())
primaryButtonC cfg buttonText = do
  (e, _) <- elAttr' "button" ("class" =: classhd <> "name" =: name) $ textS $(classh' [text_size .~ normalTextSize]) buttonText
  pure $ domEvent Click e
  where
    classhd = classhUnsafe [ bgColor .~ cfg
                           , custom .~ (primaryClass_ <&> "py-4 px-8")
                           ]
    -- for testing / selenium mainly
    name = "" -- T.filter (\c -> isAlphaNum c || isSpace c ) buttonText

primaryClass_ :: T.Text 
primaryClass_ =
  "focus:outline-none shadow-button \
  \ font-[Sarabun] font-bold text-white text-body text-center rounded-xl \
  \ active:bg-primary-desaturated \
  \ focus:ring-4 ring-primary ring-opacity-50 \
  \ transition-all duration-300 ease-in-out \
  \ transform hover:scale-105 active:scale-95 \
  \ inline-block \
  \ hover:shadow-md active:shadow-lg min-[0px]:text-xs md:text-lg"



aceLandingFooter
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m 
     )
  => m ()  
aceLandingFooter = do
  let boldWhite = textS $(classh textStyleC [text_color .~ only White, text_weight .~ only Bold, text_size .~ normalTextSize])
  let tWhite = textS $(classh textStyleC [text_color .~ only White, text_size .~ normalTextSize, custom .~ "leading-[0px]"])
  elClass "div" $(classh' [bgColor .~ only aceBlue', py .~ only (TWSize 24)]) $ do
    row [x .~ zipScreens [TWSize 20, TWSize 28, TWSize 60]] $ gridCol Col6 $ mdo
      elClass "div" $(classh' [colSpan .~ only 2]) $ do
        boldWhite "We're here to help"
        row [t .~ only (TWSize 16), r .~ only (TWSize 24)] $ do
          tWhite "For questions about the platform, sales, or any questions, feel free to contact us and we’ll be ready to assist you."
          elAttr "a" ("href" =:  "https://calendly.com/aceinterviewprep/meet-with-lauren") $ tWhite " contact us"
          tWhite " and we'll be ready to assist you."
      _ <- el "div" $ do
        boldWhite "Platform"
        row [t .~ only (TWSize 16)] $ do
          _ <- option "For Coding Bootcamps" (Landing :/ SolutionFor :/ Bootcamps :/ ())
          _ <- option "For Tech Recruiting Firms" (Landing :/ SolutionFor :/ Recruiters :/ ())
          _ <- option "For Tech Communities" (Landing :/ SolutionFor :/ Communities :/ ())
          _ <- option "For Job Seekers" (Landing :/ SolutionFor :/ JobSeekers :/ ())
          pure ()
      _ <- row [x .~ only (TWSize 4)] $ do
        boldWhite "Company"
        row [t .~ only (TWSize 16)] $ do
          _ <- option "About Us" (Landing :/ AboutUs :/ ())
          _ <- option "Blog" (Landing :/ Blog :/ BlogMain :/ ())
          pure ()
      _ <- elClass "div" $(classh' [colSpan .~ only 2, pl .~ zipScreens [TWSize 8, TWSize 0]]) $ do          
        textS $(classh textStyleC [text_color .~ only White, text_size .~ normalTextSize]) "Learn how to master interviewing"
        nlEv <- row [t .~ only (TWSize 4)] $ do
          let as = "placeholder"=: "Sign up for our newsletter..."
                   <> "class" =: "rounded-2xl inline-block w-full sm:w-[60%] p-2"
                   <> "id" =: "emailInput"
          inputEl <- inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ as
          clk <- do
            (e, _) <- elAttr' "button" ("class" =: "px-4" <> "onClick" =: "submitEmail()") $
              elClass "div" "" $ imgAttr $(static "images/Vector-white.svg") ("class" =: "w-8")
            pure $ leftmost [ domEvent Click e, keypress Enter inputEl ]
          pure $ tag (current $ value inputEl) clk --()
        row [y .~ only (TWSize 4)] $ do
          let resp = never
          dynText =<< (holdDyn "" $ "Subscribed!" <$ resp)
        tWhite "Thoughtful insights on interviewing strategies, hiring trends, and how to become a master at interviewing."
        pure nlEv

      --setRoute $ leftmost [ linkLanding , linkFor ]
      -- (_,resp :: Event t ()) <- runAPI (Api_EmailMe :/ ()) (("Newsletter Subscriber:" <>) <$> newsletterEv)
      pure ()

--renderRouteBE :: R (FullRoute BackendRoute FrontendRoute) -> T.Text
renderRouteBE :: R BackendRoute -> T.Text
renderRouteBE = Common.Elems.renderRouteBE

renderRouteFE :: R FrontendRoute -> T.Text
renderRouteFE = Common.Elems.renderRouteFE 

simpleLinkBE :: DomBuilder t m => R BackendRoute -> m a -> m a
simpleLinkBE = Common.Elems.simpleLinkBE

simpleLinkFE :: DomBuilder t m => R FrontendRoute -> m a -> m a
simpleLinkFE = Common.Elems.simpleLinkFE

aceLandingHeader
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , MonadHold t m
     )
  => m ()      
aceLandingHeader = Common.Elems.aceLandingHeader

option :: DomBuilder t m => T.Text -> (R BackendRoute) -> m (Event t (R BackendRoute))
option label sRoute = do
  let tWhite = textS $(classh textStyleC [text_color .~ only White, text_size .~ normalTextSize, custom .~ "leading-[0px]"])
  (e,_) <- simpleLinkBE sRoute $ elAttr' "div" ("class" =: $(classh' [py .~ only (TWSize 2), custom .~ "hover:underline"])) $ do
    tWhite label
  pure $ sRoute <$ domEvent Click e
      
tryAceButton :: DomBuilder t m => m (Event t ())
tryAceButton = Common.Elems.tryAceButton

bookDemoButton :: DomBuilder t m => m ()
bookDemoButton = elAttr "a" ("class" =: "bg-[#00B9DA] py-4 sm:py-6 px-4 sm:px-8 rounded focus:outline-none transition duration-300 ease-in-out hover:scale-105" <> "style" =: "text-decoration: none;" <> "href" =: "https://calendly.com/aceinterviewprep/meet-with-lauren") $ do
  textS $(classh textStyleC [ text_color .~ only White
                            , text_size .~ zipScreens [Base, Base,XL2,XL3,XL4]
                            , text_weight .~ only Bold 
                            ]
         ) "Book a demo"

loginButton  :: DomBuilder t m => m (Event t ())
loginButton = Common.Elems.loginButton--  do

linksDropdown :: (MonadHold t m, DomBuilder t m, PostBuild t m, MonadFix m) => m (Event t (R BackendRoute))
linksDropdown = Common.Elems.linksDropdown -- do

optionDropdown :: DomBuilder t m => T.Text -> R BackendRoute -> m (Event t (R BackendRoute))
optionDropdown = Common.Elems.optionDropdown -- t sRoute = do

solutionsArrow :: (PostBuild t m, DomBuilder t m) => Dynamic t Bool -> m ()
solutionsArrow = Common.Elems.solutionsArrow -- isOpen = 

bannerFor :: DomBuilder t m => T.Text -> m (Event t ()) -> m (Event t ())
bannerFor imageBackground overtop = el "div" $ do
  elClass "div" $(classh' [ custom .~ "relative", bgColor .~ only (hex "F1FCFF"), h .~ zipScreens [vh 30, vh 40, vh 80]]) $ do
    ev <- elClass "div" $(classh' [ custom .~ "absolute z-30", w .~ only TWSize_Full, h .~ only TWSize_Full, pos .~ centeredOnly]) $ do
      elClass "div" "text-center" $ overtop
    elClass "div" $(classh' [ custom .~ "z-20 absolute", w .~ only TWSize_Full, h .~ only TWSize_Full, pos .~ only (def, A_End)]) $ do
      elAttr "img" ("src" =: imageBackground <> "class" =: $(classh' [w .~ only (TWFraction 1 D3)])) blank
    pure ev

type ImgSrc = T.Text 
secondRowSolutionPage :: DomBuilder t m => ImgSrc -> T.Text -> T.Text -> m () -> m ()
secondRowSolutionPage imgSrc title desc routedButton = do
  row [y .~ only (TWSize 56)] $ do 
    row [] $ responsiveXPaddedRegion' . gridCol Col12 $ do
      elClass "div" $(classh' [colSpan .~ zipScreens [12,12,6,5], pos .~ centeredOnly]) $ do
        elClass "div" "text-center md:text-left" $ do
          row [] $ normalText "SET FOR SUCCESS"
          row [y .~ only (TWSize 4)] $ boldTitle title
          row [] $ descriptionText desc
          row [y .~ ("def",TWSize 20):("xl",TWSize 32):[]] $ routedButton
      elClass "div" $(classh' [colSpan .~ zipScreens [10,10,5,6], colStart .~ zipScreens [2,2,8,7], py .~ zipScreens [TWSize 32, TWSize 0]]) $ img imgSrc

testimonials :: DomBuilder t m => m ()
testimonials = do
  elClass "div" $(classh' [bgColor .~ only aceLightBlue]) $ do
    row [y .~ only (TWSize 28)] . responsiveXPaddedRegion' . gridCol Col2 $ do
      responsiveRowCol [2,2,1] $ responsiveXPaddedRegion' $ do
        testimonial
          "Muddassar Sharif"
          "Co-founder"
          "DevNation Coding Bootcamp"
          $(static "images/testimonial_Muddassar.webp")
          [ normalText "We invest in tools that"
          , normalBlueText "prepare our students to become top tech talent."
          , normalText "Ace is the perfect platform to provide a worry-free and valuable service to train communication skills for our students."
          ]
      responsiveRowCol [2,2,1] $ responsiveXPaddedRegion' $ do 
        testimonial
          "Patricia Musiime"
          "Career Services Officer"
          "Carnegie Mellon University Africa"
          $(static "images/testimonial_Patricia.webp") 
          [ normalText "Opting for Ace Interview Prep has proven to be a"
          , normalBlueText "transformative experience"
          , normalText "for CMU-Africa Career Services and our MS students. The platform's dynamic approach to interview preparation, combined with its remarkably user-friendly interface,"
          , normalBlueText "has exceeded our expectations."
          ]

testimonial :: DomBuilder t m => T.Text -> T.Text -> T.Text -> T.Text -> [m ()] -> m ()
testimonial name jobTitle company imgSrc quote  = do 
  row [y .~ only (TWSize 20)] $ do 
    let quoteMarkFont = textS $(classh textStyleC [text_size .~ only XL9, text_weight .~ only Extrabold, text_custom .~ "leading-3"])
    elClass "div" $(classh' [h .~ zipScreens [pix 150], custom .~ "text-center"]) $
      quoteMarkFont "“" >> intercalate (normalTextSize) " " quote
    row [t .~ only (TWSize 10)] $ do
      row [x .~ only (TWSize 0)] $ do
        gridCol Col3 $ do
          elClass "div" "" $ imgAttr imgSrc ("class" =: $(classh' [h .~ only TWSize_Full, w .~ only TWSize_Full]))
          elClass "div" $(classh' [colSpan .~ only 2, pos .~ only (J_Start, A_Center), padding . l .~ only (TWSize 10)]) $ do
            let italic = textS $(classh textStyleC [text_custom .~ "italic", text_size .~ normalTextSize ])
            row [] $ textS $(classh textStyleC [text_weight .~ only Bold, text_size .~ normalTextSize]) name
            row [] $ italic jobTitle 
            row [] $ italic company

-- responsiveXPaddedRegion :: DomBuilder t m => m a -> m a
-- responsiveXPaddedRegion = elClass "div" classCenter . elClass "div" classWidth 
--   where classWidth = $(classh'  [ w .~ zipScreens [pct 70, pct 60, pct 50, pct 40]])
--         classCenter = $(classh' [ pos .~ centeredOnly, w .~ only (pct 100) ])


                  
                              

coreValuePropForSolution :: DomBuilder t m => T.Text -> [m ()] -> T.Text -> m ()
coreValuePropForSolution title coloredText sub = do
  responsiveXPaddedRegion' . elClass "div" "py-10 md:py-48 lg:px-72 text-center" $ do
    --textS $(classh textStyleC [text_size .~ only XL4])
    normalText title 
    row [t .~ only (TWSize 8)] $ intercalate (zipScreens [XL2, XL3, XL5, XL6, XL7]) " " coloredText
    row [t .~ only (TWSize 4)] $ {-textS $(classh textStyleC [])-} normalText sub


coreValuePropForSolution' :: DomBuilder t m => T.Text -> [m ()] -> T.Text -> m ()
coreValuePropForSolution' title coloredText sub = do
  responsiveXPaddedRegion' . elClass "div" "py-10 md:py-32 lg:px-48 text-center" $ do
    --textS $(classh textStyleC [text_size .~ only XL4])
    normalText title
    row [t .~ only (TWSize 8)] $ intercalate (zipScreens [XL2, XL3, XL5, XL6, XL7]) " " coloredText
    row [t .~ only (TWSize 4)] $ {-textS $(classh textStyleC [])-} normalText sub
      

sol_4t_4p :: DomBuilder t m => Src -> T.Text -> [m ()] -> m ()
sol_4t_4p imgSrc title desc = do
  sol_shellEven $ do
    sol4_textSide J_Start title desc
    sol4_imgSide imgSrc

sol_4t_4p' :: DomBuilder t m => Src -> T.Text -> [m ()] -> m ()
sol_4t_4p' imgSrc title desc = do
  sol_shellEven' $ do
    elClass "div" $(classh' [colSpan .|~ [8,8,4], pos .~~ centered, custom .~ "hidden md:block pt-4"]) $ do
      sol4_textSide' J_Start title desc
    sol4_imgSide' imgSrc
    elClass "div" $(classh' [colSpan .|~ [8,8,4], pos .~~ centered, custom .~ "block md:hidden pt-4"]) $ do
      sol4_textSide' J_End title desc

sol_4t_4p'' :: DomBuilder t m => Src -> T.Text -> [m ()] -> m ()
sol_4t_4p'' imgSrc title desc = do
  sol_shellEven' $ do
    elClass "div" $(classh' [colSpan .|~ [8,8,5], pos .~~ centered, custom .~ "hidden md:block pt-6"]) $ do
      sol4_textSide'' J_Start title desc
    sol4_imgSide'' imgSrc
    elClass "div" $(classh' [colSpan .|~ [8,8,3], pos .~~ centered, custom .~ "block md:hidden pt-6"]) $ do
      sol4_textSide'' J_End title desc
    
sol_4p_4t :: DomBuilder t m => Src -> T.Text -> [m ()] -> m ()
sol_4p_4t imgSrc title desc = do
  sol_shellEven $ do
    sol4_imgSide imgSrc 
    sol4_textSide J_End title desc

-- TODO: Kyle edits: Review/remove this
sol_4p_4t' :: DomBuilder t m => Src -> T.Text -> [m ()] -> m ()
sol_4p_4t' imgSrc title desc = do
  sol_shellEven' $ do
    sol4_imgSide' imgSrc
    sol4_textSide' J_End title desc

sol_4p_4t'' :: DomBuilder t m => Src -> T.Text -> [m ()] -> m ()
sol_4p_4t'' imgSrc title desc = do
  sol_shellEven' $ do
    sol4_imgSide'' imgSrc
    sol4_textSide'' J_End title desc

sol4_imgSide :: DomBuilder t m => Src -> m ()
sol4_imgSide imgSrc = do
  elClass "div" $(classh' [colSpan .~ zipScreens [4], pos .~~ centered]) $ do
    imgAttr imgSrc ("class" =: $(classh' [w .~~ TWSize_Full]))

-- TODO: Kyle edits: Review/remove this
sol4_imgSide' :: DomBuilder t m => Src -> m ()
sol4_imgSide' imgSrc = do
  elClass "div" $(classh' [pos .~~ centered, colSpan .~ zipScreens [8, 8, 4], px .|~ [(TWSize 4), (TWSize 0)], pb .|~ [(TWSize 0), (TWSize 0)]]) $ do
    imgAttr imgSrc ("class" =: $(classh' [w .~~ TWSize_Full]))

sol4_imgSide'' :: DomBuilder t m => Src -> m ()
sol4_imgSide'' imgSrc = do
  elClass "div" $(classh' [pos .~~ centered, colSpan .~ zipScreens [8, 8, 3], px .|~ [(TWSize 4), (TWSize 0)], pb .|~ [(TWSize 0), (TWSize 0)]]) $ do
    elClass "div" "max-w-80 md:max-w-100" $ do
      imgAttr imgSrc ("class" =: $(classh' [w .~~ TWSize_Full]))

sol4_textSide :: DomBuilder t m => Justify -> T.Text -> [m ()] -> m ()
sol4_textSide justify title desc = do
  elClass "div" (classhUnsafe [colSpan .~ zipScreens [4], pos .~ only (justify, A_Center), px .~ only (TWSize 10)]) $ do
    el "div" . responsiveXPaddedRegion' $ do
      row [b .~ only (TWSize 10)] $ boldTitle title
      row [] $ void $ sequenceA desc -- normalText desc
      pure ()

-- TODO: Kyle edits: Review/remove this
sol4_textSide' :: DomBuilder t m => Justify -> T.Text -> [m ()] -> m ()
sol4_textSide' justify title desc = do
  elClass "div" (classhUnsafe [colSpan .|~ [8, 8, 4], pos .~~ (justify, A_Center), px .|~ [(TWSize 2), (TWSize 10)], py .|~ [(TWSize 4)]]) $ do
    el "div" . row [x .~~ TWSize 4] $ do
      row [b .|~ [(TWSize 2), (TWSize 10)]] $ boldTitleSm title
      void $ sequenceA $ row [b .~~ TWSize 8] <$> desc -- normalText desc
      pure ()

sol4_textSide'' :: DomBuilder t m => Justify -> T.Text -> [m ()] -> m ()
sol4_textSide'' justify title desc = do
  elClass "div" (classhUnsafe [colSpan .|~ [8, 8, 5], pos .~~ (justify, A_Center), px .|~ [(TWSize 2), (TWSize 10)], py .|~ [(TWSize 10), (TWSize 4)]]) $ do
    el "div" . row [x .~~ TWSize 4] $ do
      row [b .|~ [(TWSize 2), (TWSize 10)]] $ boldTitleSm title
      void $ sequenceA $ row [b .|~ [(TWSize 2), (TWSize 8)]] <$> desc -- normalText desc
      pure ()

sol_shellEven :: DomBuilder t m => m a -> m a
sol_shellEven = row [y .~~ TWSize 10] . responsiveXPaddedRegion' . gridCol Col8

-- TODO: Kyle edits: Review/remove this
sol_shellEven' :: DomBuilder t m => m a -> m a
sol_shellEven' = row [y .|~ [(TWSize 4), (TWSize 10)]] . responsiveXPaddedRegion' . gridCol Col8


sol_8t_4p :: DomBuilder t m => Src -> T.Text -> T.Text -> m ()
sol_8t_4p imgSrc title desc = do
  sol_shell $ do
    sol_textSide J_Start title desc
    sol_imgSide imgSrc
    
sol_4p_8t :: DomBuilder t m => Src -> T.Text -> T.Text -> m ()
sol_4p_8t imgSrc title desc = do
  sol_shell $ do
    sol_imgSide imgSrc 
    sol_textSide J_End title desc

sol_imgSide :: DomBuilder t m => Src -> m ()
sol_imgSide imgSrc = do
  elClass "div" $(classh' [colSpan .~ zipScreens [4]]) $ do
    imgClass imgSrc "h-64" -- ("class" =: $(classh' []))

sol_textSide :: DomBuilder t m => Justify -> T.Text -> T.Text -> m ()
sol_textSide justify title desc = do
  elClass "div" (classhUnsafe [colSpan .~ zipScreens [8], pos .~ only (justify, A_Center), px .~ only (TWSize 10)]) $ do
    el "div" . responsiveXPaddedRegion' $ do
      row [b .~ only (TWSize 10)] $ boldTitle title
      row [] $ normalText desc

sol_shell :: DomBuilder t m => m a -> m a
sol_shell = row [y .~ only (TWSize 10)] . responsiveXPaddedRegion' . gridCol Col12


statsSection :: DomBuilder t m => m () 
statsSection = do
  elClass "div" $(classh' [bgColor .~ only aceLightBlue, py .~ only (TWSize 40), px .~ zipScreens [TWSize 10, TWSize 20, TWSize 36]]) $ do
    gridCol Col3 $ do
      box "13" "tech roles offered," "from frontend to data science"
      box "90%" "confidence rate" "reported by our users after using Ace"
      box "20+" "soft skills analyzed" "to make each and every talent exceptional"
  where 
    box label ma btm = do
      elClass "div" "text-center" $ do
        let txt = textS $(classh textStyleC [text_size .~ zipScreens [XL4, XL5, XL7, XL8, XL9], text_weight .~ only Extrabold, text_color .~ only aceBlue'])
        row [b .~ only (TWSize 8)] $ txt label
        row [] $ textS $(classh textStyleC [text_size .~ normalTextSize, text_weight .~ only Bold]) ma           
        row [] $ textS $(classh textStyleC [text_size .~ normalTextSize]) btm

callToAction :: DomBuilder t m => T.Text -> T.Text -> m () -> m ()
callToAction title sub routedButton = do
  responsiveXPaddedRegion' . elClass "div" "py-48 text-center" $ do
    boldTitle title
    row [t .~ only (TWSize 8)] $ do
      normalText sub
    row [t .~ only (TWSize 10)] $ do
      routedButton

bannerTextLg, bannerTextLg', bannerTextMd, bannerTextSm, bannerTextBlue, bannerTextBlue', bannerTextMdBlue, normalText, normalTextLg, normalTextSm, normalNavyBlueText, normalBlueText, descriptionText, blueNavbarText, boldTitle, boldBlueTitle, boldTitleSm, boldBlueTitleSm
  :: DomBuilder t m => T.Text -> m ()
bannerTextLg =    textS $(classh textStyleC [text_size .|~ [XL4, XL5, XL7, XL8, XL9], text_weight .~~ Bold])
bannerTextBlue =  textS $(classh textStyleC [text_size .|~ [XL4, XL5, XL7, XL8, XL9], text_weight .~~ Bold, text_color .~~ aceBlue'])
bannerTextLg' =    textS $(classh textStyleC [text_size .|~ [XL4, XL5, XL7, XL7, XL8], text_weight .~~ Bold])
bannerTextBlue' =  textS $(classh textStyleC [text_size .|~ [XL4, XL5, XL7, XL7, XL8], text_weight .~~ Bold, text_color .~~ aceBlue'])
bannerTextMd =    textS $(classh textStyleC [text_size .|~ [XL2, XL3, XL5, XL6, XL7], text_weight .~~ Bold])
bannerTextSm =    textS $(classh textStyleC [text_size .|~ [XL, XL2, XL3, XL4, XL5], text_weight .~~ Bold])
bannerTextMdBlue =  textS $(classh textStyleC [text_size .|~ [XL2, XL3, XL5, XL6, XL7], text_weight .~~ Bold, text_color .~~ aceBlue'])
normalText =      textS $(classh textStyleC [text_size .~ normalTextSize, text_color .~~ aceNavyBlue ])
normalTextLg =    textS $(classh textStyleC [text_size .|~ [XL, XL2, XL3, XL4, XL5], text_color .~~ aceNavyBlue])
normalNavyBlueText = textS $(classh textStyleC [text_size .~ normalTextSize, text_color .~~ hex "FF823C" ])
normalBlueText =  textS $(classh textStyleC [text_size .~ normalTextSize, text_color .~~ aceBlue' ])
normalTextSm =      textS $(classh textStyleC [text_size .~ normalTextSizeSm, text_color .~~ aceNavyBlue ])
descriptionText = textS $(classh textStyleC [text_size .|~ [Base, LG, XL2, XL3, XL4], custom .~ "leading-none" ])
blueNavbarText =  Common.Elems.blueNavbarText -- textS $(classh textStyleC [text_size .|~ [SM, Base, XL2, XL2, XL4], text_color .~+ ("hover",aceBlue'):[]])
boldTitle =       textS $(classh textStyleC [text_size .~ titleSize, text_weight .~~ Bold, text_color .~~ aceNavyBlue])
boldBlueTitle =   textS $(classh textStyleC [text_size .~ titleSize, text_weight .~~ Bold,  text_color .~~ aceBlue', custom .~ "leading-normal"])
boldTitleSm =       textS $(classh textStyleC [text_size .~ titleSizeSm, text_weight .~~ Bold, text_color .~~ aceNavyBlue])
boldBlueTitleSm =   textS $(classh textStyleC [text_size .~ titleSizeSm, text_weight .~~ Bold,  text_color .~~ aceBlue', custom .~ "leading-normal"])
cvpText, cvpTextBlue :: DomBuilder t m => T.Text -> m ()
cvpText = boldTitle --textS $(classh textStyleC [text_weight .~~ Bold, text_size .~~ XL7, custom .~ "leading-normal"])
cvpTextBlue = boldBlueTitle


