module Landing.Impl where

import Landing.Static
import Lamarckian.Compiler
import Common.Route

import Language.Haskell.TH
import Obelisk.Route
import System.FilePath

import Lamarckian.Types

type Compiler = Q

atCompileTime :: a -> Compiler a
atCompileTime = pure

getPageCompiled :: R MainLandingRoute -> StaticWidget' MainLandingRoute t () -> Q Exp
getPageCompiled route widget =
  compileStaticSite site route widget
  where 
     site = StaticSite
       -- NOTE: base folder ("staticSite") must match a static assets dir
       { _staticSite_baseFilePath = "staticSite" </> "src" </> "html"
       -- TODO: split up head and body... OF THE DOM I MEAN!!
       --, _staticSite_router = staticRouter'
       , _staticSite_getFromFile = staticFilePath
       , _staticSite_routeEncoder = \r -> renderBackendRoute checkedFullRouteEncoder $ LandingR :/ r
       }


getMainPageCompiled :: R BackendRoute -> StaticWidget' BackendRoute t () -> Q Exp
getMainPageCompiled route widget =
  compileStaticSite site route widget
  where 
     site = StaticSite
       -- NOTE: base folder ("staticSite") must match a static assets dir
       { _staticSite_baseFilePath = "staticSite" </> "src" </> "html"
       -- TODO: split up head and body... OF THE DOM I MEAN!!
       --, _staticSite_router = staticRouter'
       , _staticSite_getFromFile = staticFilePath
       , _staticSite_routeEncoder = renderBackendRoute checkedFullRouteEncoder
       }


-- getPageCompiled :: R MainLandingRoute -> Q Exp
-- getPageCompiled route =
--   compileStaticSite site route
--   where 
--      site = StaticSite
--        -- NOTE: base folder ("staticSite") must match a static assets dir
--        { _staticSite_baseFilePath = "staticSite" </> "src" </> "html"
--        -- TODO: split up head and body... OF THE DOM I MEAN!!
--        , _staticSite_router = staticRouter'
--        , _staticSite_getFromFile = staticFilePath
--        , _staticSite_routeEncoder = \r -> renderBackendRoute checkedFullRouteEncoder $ LandingR :/ r
--        }
     
-- getPageCompiled' :: StaticWidget' r t () -> Q Exp
-- getPageCompiled' route =
--   compileStaticSite site page
--   where 
--      site = StaticSite
--        -- NOTE: base folder ("staticSite") must match a static assets dir
--        { _staticSite_baseFilePath = "staticSite" </> "src" </> "html"
--        -- TODO: split up head and body... OF THE DOM I MEAN!!
--        , _staticSite_router = page --staticRouter'
--        , _staticSite_getFromFile = staticFilePath
--        , _staticSite_routeEncoder = \r -> renderBackendRoute checkedFullRouteEncoder $ LandingR :/ r
--        }

