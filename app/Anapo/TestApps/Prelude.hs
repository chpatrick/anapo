{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Anapo.TestApps.Prelude
  ( module X
  , module Anapo.TestApps.Prelude
  ) where

import Control.Monad.IO.Class as X (liftIO)
import qualified Data.Text as T

import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.HTMLInputElement as DOM

import Anapo

#if !defined(ghcjs_HOST_OS)
import Language.Javascript.JSaddle.Monad (JSM(..))
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Reader (ask, runReaderT)

deriving instance MonadBase IO JSM

instance MonadBaseControl IO JSM where
  type StM JSM a = a
  {-# INLINE liftBaseWith #-}
  liftBaseWith cont = do
    x <- JSM ask
    liftIO (cont (\(JSM m) -> runReaderT m x))
  {-# INLINE restoreM #-}
  restoreM = return
#endif

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

bootstrapRow :: Component read write -> Component read write
bootstrapRow = n . div_ (class_ "row")

bootstrapCol :: Component read write -> Component read write
bootstrapCol = n . div_ (class_ "col")

booleanCheckbox :: Node' DOM.HTMLInputElement Bool
booleanCheckbox = do
  st <- askState
  dispatch <- askDispatch
  input_
    (type_ "checkbox")
    (checked_ st)
    (onchange_ $ \el ev -> do
      DOM.preventDefault ev
      checked <- DOM.getChecked el
      dispatch (const checked))

simpleTextInput ::
     ClientM ()
  -- ^ what to do when the new text is submitted
  -> T.Text
  -- ^ what to show in the button
  -> Component' T.Text
simpleTextInput cback buttonTxt = do
  currentTxt <- askState
  dispatch <- askDispatch
  n$ form_
    (onsubmit_ $ \_ ev -> do
      DOM.preventDefault ev
      cback)
    (do
      n$ input_
        (value_ currentTxt)
        (oninput_ $ \inp _ -> do
          txt <- DOM.getValue inp
          dispatch (const txt))
      n$ button_ (n$ text buttonTxt))

{-
data ReadOptionState = ReadOptionState
  {
readOption :: (Show a, Read a) => Node' a
-}
