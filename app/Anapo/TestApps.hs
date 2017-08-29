{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module Anapo.TestApps (TestAppsState, testAppsComponent, testAppsInit) where

import Control.Applicative
import Control.Lens (makeLenses, set, (^.), Lens', forOf_, folded)
import Control.Monad (guard)
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
  type RouteState r :: *
  tryRoute :: [ JSString.JSString ] -> Maybe (r -> Component' (RouteState r))
  default tryRoute :: (Generic r, Router (Rep r ()), RouteState (Rep r ()) ~ RouteState r) => [ JSString.JSString ] -> Maybe (r -> Component' (RouteState r))
  tryRoute = tryRouteNext (\router -> from router :: Rep r ())

tryRouteNext :: Router r' => (r -> r') -> [ JSString.JSString ] -> Maybe (r -> Component' (RouteState r'))
tryRouteNext f segs = do
  routeNext <- tryRoute segs
  return (routeNext . f)

class Abbreviated a where
  type Brief a :: *
  type instance Brief a = a
  brief :: Brief a -> a
  default brief :: a -> a
  brief = id

newtype Seg (seg :: Symbol) next = Seg { segNext :: next }
newtype Capture a next = Capture { captureNext :: a -> next }
newtype End state = End { endComponent :: Component' state }
data Zoom out next = Zoom
  { zoomLens :: Lens' out (RouteState next)
  , zoomNext :: next
  }

type a :> b = a b
infixr 2 :>

type seg /> b = Seg seg b
infixr 2 />

instance Router (End state) where
  type RouteState (End state) = state
  tryRoute segs = endComponent <$ guard (null segs)

instance Abbreviated (End state) where
  type Brief (End state) = Component' state
  brief = End

instance (KnownSymbol seg, Router next) => Router (Seg seg next) where
  type RouteState (Seg seg next) = RouteState next
  tryRoute (s : nextPath)
    | JSString.unpack s == symbolVal (Proxy :: Proxy seg) = tryRouteNext segNext nextPath
  tryRoute _ = Nothing

instance Abbreviated next => Abbreviated (Seg seg next) where
  type Brief (Seg seg next) = Brief next
  brief = Seg . brief

instance Router next => Router (Zoom out next) where
  type RouteState (Zoom out next) = out
  tryRoute segs = do
    routeNext <- tryRoute segs
    return $ \(Zoom l next) -> zoom' l (routeNext next)

instance Abbreviated next => Abbreviated (Zoom out next)

instance Router r => Router (K1 i r p) where
  type RouteState (K1 i r p) = RouteState r
  tryRoute = tryRouteNext unK1

instance Router (f p) => Router (M1 i c f p) where
  type RouteState (M1 i c f p) = RouteState (f p)
  tryRoute = tryRouteNext unM1

instance (Router (left p), Router (right p), RouteState (left p) ~ RouteState (right p)) => Router ((left :*: right) p) where
  type RouteState ((left :*: right) p) = RouteState (left p)
  tryRoute segs = routeLeft <|> routeRight
    where
      routeLeft = tryRouteNext (\(left :*: _) -> left) segs
      routeRight = tryRouteNext (\(_ :*: right) -> right) segs

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

routerComponent :: Router r => r -> Component' (RouterState (RouteState r))
routerComponent router = do
  dispatch <- askDispatch
  st <- askState

  let updateRoute = do
        window <- DOM.currentWindowUnchecked
        loc <- DOM.getLocation window
        pathName <- DOM.getPathname loc
        let segs = dropWhile JSString.null $ JSString.splitOn "/" pathName
        putStrLn ("segs: " ++ show segs)
        let mbComponent = fmap ($ router) $ tryRoute segs
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

data TestAppRoutes = TestAppRoutes
  { routesTodo :: ("todo" /> Zoom TestAppsState :> End TodoState )
  , routesTimer :: ("timer" /> Zoom TestAppsState :> End TimerState)
  , routesYoutube :: ("hogjowls" /> Zoom TestAppsState :> End YouTubeState)
  , routesBlank :: End TestAppsState
  } deriving (Generic)
instance Router TestAppRoutes where
  type RouteState TestAppRoutes = TestAppsState

testAppRoutes :: TestAppRoutes
testAppRoutes = TestAppRoutes
  { routesTodo = brief $ Zoom tasTodo $ End todoComponent
  , routesTimer = brief $ Zoom tasTimer $ End timerComponent
  , routesYoutube = brief $ Zoom tasYouTube $ End youTubeComponent
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