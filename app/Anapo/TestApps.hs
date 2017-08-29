{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module Anapo.TestApps (TestAppsState, testAppsComponent, testAppsInit) where

import Control.Applicative
import Control.Lens (makeLenses, set, (^.), traverseOf, Lens', forOf_, folded)
import Control.Monad (forM_, guard)
import Data.IORef
import Data.Maybe
import qualified Data.JSString as JSString
import Data.Proxy
import GHC.TypeLits
import GHC.Generics

import Anapo
import Anapo.TestApps.Prelude
import Anapo.TestApps.TodoList
import Anapo.TestApps.Timer
import Anapo.TestApps.YouTube
import qualified Anapo.VDOM as V

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.HTMLSelectElement as DOM
import qualified GHCJS.DOM.HTMLDivElement as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import qualified GHCJS.DOM.PopStateEvent as DOM
import qualified GHCJS.DOM.Location as DOM
import qualified GHCJS.DOM.Window as DOM

class Router r where
  tryRoute :: [ JSString.JSString ] -> Maybe (r state -> Component' state)
  default tryRoute :: (Generic1 r, Router (Rep1 r)) => [ JSString.JSString ] -> Maybe (r state -> Component' state)
  tryRoute = tryRouteNext from1
  zoomRoute :: Lens' out in_ -> r in_ -> r out
  default zoomRoute :: (Generic1 r, Router (Rep1 r)) => Lens' out in_ -> r in_ -> r out
  zoomRoute l = to1 . zoomRoute l . from1

class Abbreviated a where
  type Brief a :: *
  type instance Brief a = a
  brief :: Brief a -> a
  default brief :: a -> a
  brief = id

newtype Seg (seg :: Symbol) next state = Seg { segNext :: next state }
newtype Capture a next state = Capture { captureNext :: a -> next state }
newtype End state = End { endComponent :: Component' state }

type a :> b = a b
infixr 2 :>

type seg /> b = Seg seg b
infixr 2 />

tryRouteNext :: Router r' => (r state -> r' state) -> [ JSString.JSString ] -> Maybe (r state -> Component' state)
tryRouteNext f path = do
  routeNext <- tryRoute path
  return (routeNext . f)

instance Router End where
  tryRoute segs = endComponent <$ guard (null segs)
  zoomRoute l (End comp) = End (zoom' l comp)

instance Abbreviated (End state) where
  type Brief (End state) = Component' state
  brief = End

instance (KnownSymbol seg, Router next) => Router (Seg seg next) where
  tryRoute segs =
    case segs of
      (s : nextPath) | JSString.unpack s == symbolVal (Proxy :: Proxy seg) -> tryRouteNext segNext nextPath
      _ -> Nothing
  zoomRoute l (Seg next) = Seg (zoomRoute l next)

instance Abbreviated (next state) => Abbreviated (Seg seg next state) where
  type Brief (Seg seg next state) = Brief (next state)
  brief = Seg . brief

deriving instance Router f => Router (Rec1 f)
deriving instance Router f => Router (M1 i c f)

instance (Router left, Router right) => Router (left :*: right) where
  tryRoute req = routeLeft <|> routeRight
    where
      routeLeft = tryRouteNext (\(left :*: _) -> left) req
      routeRight = tryRouteNext (\(_ :*: right) -> right) req
  zoomRoute l (left :*: right) = zoomRoute l left :*: zoomRoute l right

data RouterState state = RouterState
  { _routerMountState :: IORef (Maybe (IO ()))
  , _routerCurrentComponent :: Maybe (Component' state)
  , _routerInnerState :: state
  }
makeLenses ''RouterState

routerInit :: state -> ClientM (RouterState state)
routerInit initState = do
  mountState <- newIORef Nothing
  return RouterState
    { _routerMountState = mountState
    , _routerCurrentComponent = Nothing
    , _routerInnerState = initState
    }

routerComponent :: Router r => r state -> Component' (RouterState state)
routerComponent router = do
  dispatch <- askDispatch
  st <- askState

  let updateRoute = do
        window <- DOM.currentWindowUnchecked
        loc <- DOM.getLocation window
        pathName <- DOM.getPathname loc
        let segs = dropWhile JSString.null $ JSString.splitOn "/" pathName
        putStrLn ("segs: " ++ show segs)
        let mbComponent = ($ router) <$> tryRoute segs
        putStrLn ("mbComponent: " ++ show (isJust mbComponent))
        dispatch (set routerCurrentComponent mbComponent)

  let setup _ = do
        window <- DOM.currentWindowUnchecked
        document <- DOM.currentDocumentUnchecked
        putStrLn "setup"
        releaseOnPopState <- DOM.on window DOM.popState $ liftIO $ updateRoute
        releaseOnClick <- DOM.on document DOM.click $ liftIO $ updateRoute

        writeIORef (st ^. routerMountState) (Just (releaseOnPopState >> releaseOnClick))
        updateRoute

  let cleanup _ = do
        putStrLn "cleanup"
        mbCleanup <- readIORef (st ^. routerMountState)
        sequence_ mbCleanup

  n$ do
    wrapper <- div_ $
      forOf_ (routerCurrentComponent . folded) st $ zoom' routerInnerState

    return $ wrapper
      { V.nodeCallbacks = mempty
        { V.callbacksUnsafeDidMount = setup
        , V.callbacksUnsafeWillRemove = cleanup
        }
      }

data WhichTestApp =
    Blank
  | Todo
  | Timer
  | YouTube
  deriving (Eq, Show, Read)

data TestAppsState = TestAppsState
  { _tasWhich :: WhichTestApp
  , _tasTodo :: TodoState
  , _tasTimer :: TimerState
  , _tasStopTimerOnAppChange :: Bool
  , _tasYouTube :: YouTubeState
  }
makeLenses ''TestAppsState

data TestAppRoutes state = TestAppRoutes
  { routesTodo :: ("todo" /> End) state
  , routesTimer :: ("timer" /> End) state
  , routesYoutube :: ("hogjowls" /> End) state
  , routesBlank :: End state
  } deriving (Generic1)
instance Router TestAppRoutes

testAppRoutes :: TestAppRoutes TestAppsState
testAppRoutes = TestAppRoutes
  { routesTodo = zoomRoute tasTodo $ brief todoComponent
  , routesTimer = zoomRoute tasTimer $ brief timerComponent
  , routesYoutube = zoomRoute tasYouTube $ brief youTubeComponent
  , routesBlank = brief (return ())
  }

testAppsComponent :: Component' (RouterState TestAppsState)
testAppsComponent = routerComponent testAppRoutes

{-
testAppsComponent :: Component' TestAppsState
testAppsComponent = do
  dispatchM <- askDispatchM
  st <- askState
  bootstrapRow $ do
    bootstrapCol $ do
      n$ "Choose an app:"
      n$ select_
        (onchange_ $ \el _ -> do
          newApp <- read <$> DOM.getValue el
          dispatchM $ \st' -> do
            st'' <- if st^.tasStopTimerOnAppChange && newApp /= Timer
              then traverseOf tasTimer timerStop st'
              else return st'
            return (set tasWhich newApp st''))
        (forM_ [Blank, Todo, Timer, YouTube] $ \which -> do
          n$ option_
            (value_ (tshow which))
            (selected_ (which == st ^. tasWhich))
            (n$ text (tshow which)))
    bootstrapCol $ do
      zoom' tasStopTimerOnAppChange (n$ booleanCheckbox)
      n$ "Stop timer app when changing app"
  bootstrapRow $ bootstrapCol $ case st^.tasWhich of
    Blank -> return ()
    Todo -> zoom' tasTodo todoComponent
    Timer -> zoom' tasTimer timerComponent
    YouTube -> zoom' tasYouTube youTubeComponent
-}

testAppsInit :: ClientM (RouterState TestAppsState)
testAppsInit = do
  innerState <-
    TestAppsState
      <$> pure Todo
      <*> todoInit
      <*> timerInit
      <*> pure False
      <*> youTubeInit "Hah4iGqh7GY"

  routerInit innerState