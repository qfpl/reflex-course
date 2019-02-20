{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module TodoMVC (
    todoMVC
  ) where

import Control.Monad (guard)
import Data.Bool (bool)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core
import Reflex.Dom.Contrib.Vanishing

data TodoItem = TodoItem { _tiText :: Text, _tiComplete :: Bool }

hideOnZero :: Int -> ElementVisibility
hideOnZero =
  bool Visible VisibilityHidden . (== 0)

todoMVC :: forall t m. MonadWidget t m => m ()
todoMVC = do
  elClass "section" "todoapp" $ do
    eAdd <- header

    eCount <- numberOccurrences eAdd
    dMap <- foldDyn ($) mempty . mergeWith (.) $
      [ (\(k, t) -> Map.insert k (TodoItem t False)) <$> eCount
      ]

    _ <- main dMap

    eClearComplete <- footer (Map.size <$> dMap) (Map.size . Map.filter _tiComplete <$> dMap)

    pure ()

header :: MonadWidget t m => m (Event t Text)
header = do
  elClass "header" "header" $ mdo
    el "h1" $ text "todos"
    i <- inputElement $
      def & initialAttributes .~ ("class" =: "new-todo" <>
                                  "placeholder" =: "What needs to be done?" <>
                                  "autofocus" =: "")
          & inputElementConfig_setValue .~ ("" <$ eEnter)
    let
      eEnter = fmapMaybe (\n -> guard $ keyCodeLookup (fromIntegral n) == Enter) . domEvent Keypress $ i
      eText = Text.strip <$> current (value i) <@ eEnter
      eAdd = ffilter (not . Text.null) eText
    pure eAdd

main :: MonadWidget t m => Dynamic t (Map Int TodoItem) ->  m (Event t Bool)
main dMap = do
  let
    dVisAttrs = hideOnZero . Map.size <$> dMap
  vanishingAttr "section" ("class" =: "main") dVisAttrs $ do
    eMarkAllAsComplete <- markAllAsComplete (pure True)
    _ <- elClass "ul" "todo-list" . listWithKey dMap $ \k dv ->
      -- need to add the editing class while editing
      elDynAttr "li" (bool mempty ("class" =: "completed") . _tiComplete <$> dv) $
        item dv
    pure eMarkAllAsComplete

item :: MonadWidget t m => Dynamic t TodoItem -> m ()
item dItem = do
  dText <- holdUniqDyn $ _tiText <$> dItem
  iText <- sample . current $ dText

  divClass "view" $ do
    blank
    el "label" $ dynText dText
    blank

  i <- inputElement $
    def & initialAttributes .~ ("class" =: "edit")
        & inputElementConfig_initialValue .~ iText

  pure ()

markAllAsComplete :: MonadWidget t m => Dynamic t Bool -> m (Event t Bool)
markAllAsComplete dAllComplete = do
  i <- inputElement $
    def & initialAttributes .~ ( "id" =: "toggle-all" <>
                                 "class" =: "toggle-all" <>
                                 "type" =: "checkbox"
                               )
        & inputElementConfig_initialChecked .~ False
        & inputElementConfig_setChecked .~ updated dAllComplete
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"

  pure $ _inputElement_checkedChange i

footer :: MonadWidget t m => Dynamic t Int -> Dynamic t Int -> m (Event t ())
footer dNumTodos dNumComplete = do
  let
    dVisAttrs = hideOnZero <$> dNumTodos
  vanishingAttr "footer" ("class" =: "footer") dVisAttrs $ do
    eClearComplete <- clearCompleted dNumComplete
    pure eClearComplete

clearCompleted :: MonadWidget t m => Dynamic t Int -> m (Event t ())
clearCompleted dNumComplete = do
  let
    attrs = "class" =: "clear-completed"
    dVisAttrs = visibilityAttrs . hideOnZero <$> dNumComplete
  (l, _) <- elDynAttr' "button" (dVisAttrs <> pure attrs) $
    text "Clear completed"
  pure $ domEvent Click l
