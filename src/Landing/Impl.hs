module Landing.Impl where

import Landing.Static
import Lamarckian.Compiler
import Landing.Router
import Common.Route

import Language.Haskell.TH
import Obelisk.Route
import System.FilePath

getPageCompiled :: R LandingRoute -> Q Exp
getPageCompiled route =
  compileStaticSite site route
  where 
     site = StaticSite
       -- NOTE: base folder ("staticSite") must match a static assets dir
       { _staticSite_baseFilePath = "staticSite" </> "src" </> "html"
       -- TODO: split up head and body... OF THE DOM I MEAN!!
       , _staticSite_router = staticRouter'
       , _staticSite_getFromFile = staticFilePath
       , _staticSite_routeEncoder = \r -> renderBackendRoute checkedFullRouteEncoder $ Landing :/ r
       }
     
     -- staticRouter
     -- staticFilePath
     -- (\r -> renderBackendRoute checkedFullRouteEncoder $ Landing :/ r)
     -- $ route


-- TODO: currently this gives all power to staticRouter to set the head
-- it would be nice to abstract an interface like this

-- runStaticSite :: StaticSite t route -> R route -> Q Exp

-- data StaticSite t route = StaticSite
--   { staticDir :: FilePath
--   , frontendHead :: R route -> StaticWidget' t ()
--   , frontendBody :: R route -> StaticWidget' t ()
---  maybe more:
--   , snapOptions :: OptionsSnap 
--   }

