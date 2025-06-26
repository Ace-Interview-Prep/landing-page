{-# LANGUAGE UndecidableInstances #-}

module Landing.Pages.ContactUsForm where

import Landing.Classh
import Landing.Static
import Landing.Utils
import Control.Monad.Trans.Class

import Templates.Partials.Image
import Classh
import Classh.Box.TWSize
import Classh.Reflex

import Reflex.Dom
import Control.Monad.Trans.State
import Control.Monad (void, when, forM_)
import qualified Data.Text as T

contactUsFormPage :: DomBuilder t m => m ()
contactUsFormPage = do
  let colorsss = [Green C300, Blue C700, Pink C300, Rose C900, Orange C900, Black]
  let xs = zip Classh.sizes colorsss

  el "div" $ do
    whatSizeAmIRow colorsss
    forM_ xs $ \cs -> do
      div_ . text . tshow $ cs

  contactUsFormRow
  bookConsultationRow

  where
    -- A visual cue to help debugging by showing the context (via color)
    whatSizeAmIRow c = divClass (classhUnsafe [ w .~~ pct 100
                                              , bgColor .|~ c
                                              ]) $ text "_"


editorCanvas' :: DomBuilder t m => Shell m a
editorCanvas' dom = do
  placeCenterWidth [("def", pct 40)] $ do
    el "x" $ do
      elClass "div" "" $ do
        dom

--twConst  :: ?




--testClasshV' = $(classhV' p .- pix 300 <> m .- pix 100 <> bgColor .|<~ [White,Black])



bookConsultationRow :: DomBuilder t m => m ()
bookConsultationRow = do
  bookConsultationRowTop `overtopOf_` bookConsultationRowBottom
  where
    overtopOf_ = overtopOf [vh 55]

bookConsultationRowBottom :: DomBuilder t m => m ()
bookConsultationRowBottom = do
  divClass $(classh' [bgColor .~~ hex "F7F8FA", p .~~ TWSize 16, h .~~ pct 100]) $ do 
    imgClass $(staticImg "matrix-mesh-top.png") $(classh' [w .~~ pct 100])
    imgClass $(staticImg "matrix-mesh-bottom.png") $(classh' [pt .~~ TWSize 0, w .~~ pct 100])


  
bookConsultationRowTop :: DomBuilder t m => m ()
bookConsultationRowTop = divClass (classhUnsafe centeredFullH) $ do
  padText $ do
    title43 "Book your consultation"
  row [t .~~ TWSize 6] . padText $ do
    placeCenterWidth (zipSizes [pct 90, pct 70]) $ textPosition $(classh' [tp_position .~~ (TA_Center,TA_Mid)]) $ subtext16
      "Book your free 1-hour consultation on global hiring and find out why it might be the smartest move you make."
  row [t .~~ TWSize 10] $ do
    divClass $(classh' [w .|~ [pix 300, pix 400]]) $ do
      buttonSubmitNow "Schedule a consultation"
  where
    --padText' = (placeCenterWidth . zipSizes . fmap pct $ [80,90,100]) . divClass "text-center"
    padText = (placeCenterWidth . zipSizes . fmap pct $ [80,90,100]) . divClass "text-center"
    
contactUsFormRow :: DomBuilder t m => m ()
contactUsFormRow = do
  contactUsFormRowTop `overtopOf_` contactUsFormRowBottom
  where
    overtopOf_ = overtopOf [vh 120, vh 120, vh 100]
    -- TODO: overtopOf_ :: ContainerHeights -> TopPlacementHeights -> BottomPlacementHeights
    
zipSizes :: [a] -> WhenTW a
zipSizes = zip sizes
  
contactUsFormRowTop :: DomBuilder t m => m ()
contactUsFormRowTop = divClass $(classh' [h .~~ vh 100]) $ centerSimple $ do
  gridColW Col12 (pct 100) $ do
    colFrom [(0,12),(0,12),(2,5)] $ divClass $(classh' [ w .|~ (pct <$> [90]) ]) $ do 
      row [] $ annotation14 "Contact Us"
      row [t .~~ TWSize 16] . padText $ do
        title58
         "Let's Chat Talent"
      row [t .~~ TWSize 6] . padText $ do
        subtext16 "Connect with our team to learn more about hiring your next engineer, how our community works, or anything else about building world-class teams."
      row [t .~~ TWSize 12] . padText $ do
        subtext16 "We know what great talent looks like. We can’t wait to share that with you."
      row [t .~~ TWSize 12] . padText $ do
        subtext16 "Connect with us on social media:"
      row [t .~~ TWSize 10] . padText $ divClass "text-left" $ do
        inlines [linkedInButton, tiktokButton, instagramButton]
    col [12,12,6] $ do
      divClass $(classh' [pos .|~ [(J_Center, A_Center)], w .~~ pct 100]) $ do 
        contactUsForm
  where
    padText = (placeCenterWidth . zipSizes . fmap pct $ [80,90,100]) . divClass "text-left" 

-- instance DomBuilder t m => DomBuilder t (StateT r m) where
--   type DomBuilderSpace (StateT r m) = DomBuilderSpace m
  
test :: DomBuilder t m => StateT Int m ()
test = lift $ divClass "" $ text "hey"

contactUsFormRowBottom :: DomBuilder t m => m ()
contactUsFormRowBottom = do
  divClass $(classh' [pos .~~ (J_End,A_End), h .~~ pct 100, bgColor .~~ hex "F7F8FA"]) $ do
    imgClass $(staticImg "globe-background.png") ""

linkedInButton, tiktokButton, instagramButton :: DomBuilder t m => m ()
linkedInButton = socialShell' $ imgClass $(staticImg "linkedin.png") $(classh' [p .~~ TWSize 3])
tiktokButton = socialShell $ imgClass $(staticImg "tiktok.png") $(classh' [p .~~ TWSize 3])
instagramButton = socialShell $ imgClass $(staticImg "insta.png") $(classh' [p .~~ TWSize 3])

socialShell :: DomBuilder t m => Shell m a
socialShell = divClass $(classh' [br .~~ R_Full, bw .~~ B2, bc .~~ Gray C200, ml .~~ TWSize 8])

socialShell' :: DomBuilder t m => Shell m a
socialShell' = divClass $(classh' [br .~~ R_Full, bw .~~ B2, bc .~~ Gray C200])

-- -- | A generic interface to creating a centered row of some width+width constraints
-- rowH :: DomBuilder t m => TWSizeOrFraction -> (Justify, Align) -> m a -> m a 
-- rowH heightCfg pos_ = centerSimple . divClass (classhUnsafe [ h .~~ heightCfg ])


--inputEl :: [Mutation BoxConfig] ->
--inputEl



-- | this is a test
-- ![Diagram](https://messinas.com/cdn/shop/articles/The_Best_Methods_to_Deweed_Lawn_Quickly_Messinas.jpg?v=1727628042&width=1920https://messinas.com/cdn/shop/articles/The_Best_Methods_to_Deweed_Lawn_Quickly_Messinas.jpg?v=1727628042&width=1920)  
contactUsForm :: DomBuilder t m => m ()
contactUsForm = do
  bigBorder R_3Xl (TWSize 1.5) White $ do 
    divClass containerClass $ do
      row1 >> row2 >> row3 >> buttonSubmitNow "Submit Now"
  where
    bigBorder :: DomBuilder t m => BorderRadius' -> TWSize -> Color -> Shell m a
    bigBorder r s c = divClass (classhUnsafe [ p .~~ s, bgColor .~~ c, br .~~ r, w .~~ pct 85])
    containerClass = $(classh' [ p .~~ TWSize 16
                               , br .~~ R_3Xl
                               , bc .~~ Gray C300
                               , bw .~~ B2
                               ]) 
    row1 = do
      divClass $(classh' [w .~~ pct 100, pos .~~ (J_Start, A_Center)]) $ do 
        inputEl
          $(classh' [bgColor .~~ hex "FAFAFC", w .~~ pct 100, p .~~ TWSize 4])
          placeholder18
          "Full Name*"
    row2 = do 
      row [y .~~ TWSize 6] $ gridColW Col12 (pct 100) $ do
        col [12,12,12,6] $ do
          inputEl
            $(classh' [w .~~ pct 90, p .~~ TWSize 4, pos .~~ (J_Start, A_Center)])
            placeholder18
            "Phone Number*"
        col [12,12,12,6] $ row [t .|~ (TWSize <$> [6, 6, 0])] $ do
          inputEl
            $(classh' [w .~~ pct 100, p .~~ TWSize 4, pos .~~ (J_Start, A_Center)])
            placeholder18
            "Email*"
    row3 = do 
      divClass $(classh' [py .~~ TWSize 6, w .~~ pct 100]) $ do
        textAreaEl 10
          $(classh' [w .~~ pct 100, p .~~ TWSize 4])
          placeholder18
          "How can we help?*"

buttonSubmitNow :: DomBuilder t m => T.Text -> m ()
buttonSubmitNow txt = do 
  divClass "bg-gradient-to-r from-[#14a9db] to-[#2A89DC] rounded-full h-20" $ do
    divClass $(classh' [pos .~~ centered, h .~~ pct 100]) $ do
      inH_ $ do
        divClass $(classh' [h .~~ pct 100, p .~~ TWSize 0, custom .~ "inline-block"])  $ do
          divClass $(classh' [h .~~ pct 100, pos .~~ centered]) $ do
            buttonText14 txt
        divClass $(classh' [pl .~~ TWSize 8, custom .~ "inline-block", h .~~ pct 100]) $ in_ $ do
          divClass $(classh' [h .~~ pct 100, pos .~~ (J_Center,A_End)]) $ do
            imgClass
              $(staticImg "bent-arrow-from_down-to_right.png")
              $(classh' [h .~~ pct 92]) 

title58, title43, subtext16, annotation14, buttonText14 :: DomBuilder t m => T.Text -> m () 
title58 = textS $(classh' [text_size .~~ XL7, text_weight .~~ Bold])
title43 = textS $(classh' [text_size .~~ XL7, text_weight .~~ Bold])
subtext16 = textS $(classh' [text_size .|~ [XL2,XL3]])
placeholder18 :: CompiledS
placeholder18 = $(classh' [text_size .~~ XL3])
annotation14 = textS $(classh' [ text_size .~~ XL3
                               , custom .~ "bg-gradient-to-r from-[#00c6ff] to-[#0072ff] bg-clip-text text-transparent"
                               ])
buttonText14 = textS $(classh' [text_size .~~ XL4, text_color .~~ White]) 


-- imgResponsive
--   :: DomBuilder t m
--   => [ (StaticImagePath
--        , (CSSSize, MaxWidth)
--        )
--      ]
--   -> CompiledS
--   -> m ()
-- imgResponsive = elAttr "img" (imgSrcSet  [( $(staticImg "logo-single-blue.webp") ,(,) )] "") blank

example :: DomBuilder t m => m ()
example = imgResponsive images "" 
  where
    images = [ ( $(staticImg "logo-single-blue.webp") , (pix 300, DC_none) )
             , ( $(staticImg "logo-single-blue.webp") , (pix 300, DC_none) )
             ]
