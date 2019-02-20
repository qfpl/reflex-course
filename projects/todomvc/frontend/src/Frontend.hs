{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import TodoMVC

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head =  do
    el "title" $ text "Todo MVC"
    elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"style.css") blank
  , _frontend_body = prerender (text "Loading...") $ do
      todoMVC
  }
