{-# LANGUAGE OverloadedStrings #-} 
module Landing.Utils where

-- probably mainly JS stuff 
import Common.Route
import Obelisk.Route.Frontend
import Language.Haskell.TH (Q, Exp)
import Landing.Static
import qualified Data.Text as T

-- runEmailWidgetWithRefs :: (HTemplateRefs -> StaticWidget' x a) -> IO BS.ByteString 
-- runEmailWidgetWithRefs =
--   fmap snd . renderStatic . flip runRouteToUrlT renderRouteForEmail . runSetRouteT


-- | Find a more logical module for these
staticVideo, staticImg :: FilePath -> Q Exp
staticImg img = static $ "images/" <> img
staticVideo video = static $ "videos/" <> video

renderRouteForEmail :: R FrontendRoute -> T.Text
renderRouteForEmail = ("" <>) . renderFrontendRoute checkedFullRouteEncoder
