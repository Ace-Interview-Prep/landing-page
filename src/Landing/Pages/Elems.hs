{-# LANGUAGE OverloadedStrings #-}

module Landing.Pages.Elems where

import System.IO.Unsafe
import Control.Exception

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







-- Note: this should only ever be used in a static dom builder Template Haskell context 
has3 :: [JSFunc] -> (JSFunc,JSFunc,JSFunc)
has3 (x_:y_:z_:[]) = (x_,y_,z_)
has3 _ = do
  unsafePerformIO $ do
    appendFile "exception.txt" "EXCEPTION"
    error "incorrect number of dom setters"


-- -- | A dumb shell which does nothing but visually wrap a DomBuilder
-- -- | is for documentation purposes
type DomShell m a = m a -> m a

--renderRouteBE :: R (FullRoute BackendRoute FrontendRoute) -> T.Text
renderRouteBE :: R BackendRoute -> T.Text
renderRouteBE = Common.Elems.renderRouteBE

renderRouteFE :: R FrontendRoute -> T.Text
renderRouteFE = Common.Elems.renderRouteFE 

simpleLinkBE :: DomBuilder t m => R BackendRoute -> m a -> m a
simpleLinkBE = Common.Elems.simpleLinkBE

simpleLinkFE :: DomBuilder t m => R FrontendRoute -> m a -> m a
simpleLinkFE = Common.Elems.simpleLinkFE

