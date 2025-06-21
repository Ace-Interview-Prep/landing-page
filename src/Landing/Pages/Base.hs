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


type Highlighted = Bool

data Testimonial = Testimonial
  { _testimonial_name :: T.Text
  , _testimonial_jobTitle :: T.Text
  , _testimonial_orgName :: T.Text
  , _testimonial_headshot :: T.Text 
  , _testimonial_texts :: [(Highlighted, T.Text)]
  }
  
data Feature = Prepare | Practice | Progress deriving (Eq, Show)

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

