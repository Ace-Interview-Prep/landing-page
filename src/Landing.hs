{-# LANGUAGE QuasiQuotes #-}
module Landing where

import Reflex.Dom.Core

import Landing.Impl
import Landing.Pages.ContactUsForm
import Lamarckian.Snap
import Landing.MyShell

import Common.Route
import Obelisk.Route

import Snap
import Snap.Util.GZip
import Language.Haskell.TH
import Control.Monad (void)
import Control.Monad.IO.Class
import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS


-- | Route Values/Literals still need to operate independently of each other 


selectLandingPage :: MonadSnap m => R MainLandingRoute -> m ()
selectLandingPage r = serve $ case r of
  ContactUsForm :/ _ -> Right $(getPageCompiled (ContactUsForm :/ ()) $ mainShell contactUsFormPage)
  _ -> Left ""  -- pure ()
  where
    serve :: MonadSnap m => Either T.Text FilePath -> m () 
    serve = \case
      Left err -> liftIO $ print err
      Right fp -> void . serveCompressed $ fp

-- | This is on a seperate route tree so we need a second config
getMainLandingPage :: MonadSnap m => m ()
getMainLandingPage = void . serveCompressed $
  $(getMainPageCompiled (LandingBase :/ ()) $ do
       text "Main Page"
   )
