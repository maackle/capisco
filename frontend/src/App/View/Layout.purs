module App.View.Layout where

import Prelude hiding (div)

import App.Events (Event)
import App.Routes (Route(NotFound, Home))
import App.State (State)
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import CSS (CSS, backgroundColor, borderRadius, color, display, fontSize, fromString, inlineBlock, key, marginRight, marginTop, maxWidth, padding, pct, px, value, (?))
import CSS.Common (none)
import CSS.ListStyle.Type (listStyleType)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (center, textAlign)
import Color (rgb)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))


view :: State -> HTML Event
view st =
  div ! className "app" $ do
    style css
    case st.routing.route of
      (Home) -> Homepage.view st
      (NotFound url) -> NotFound.view st

css :: CSS
css = do
  let red = rgb 196 14 172
      green = rgb 14 196 172
      blue = rgb 14 154 196
      white = rgb 250 250 250
      gray = rgb 180 180 180

  fromString "body" ? do
    backgroundColor (rgb 240 240 240)
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")
    color (rgb 30 30 30)

  fromString "h1" ? do
    fontSize (48.0 #px)
    marginTop (48.0 #px)
    textTransform uppercase
    letterSpacing (6.0 #px)
    textAlign center

  fromString "ul" ? do
    listStyleType none

  fromString ".container" ? do
    maxWidth (80.0 #pct)

  fromString "a" ? do
    display inlineBlock
    borderRadius (2.0 #px) (2.0 #px) (2.0 #px) (2.0 #px)
    padding (6.0 #px) (6.0 #px) (6.0 #px) (6.0 #px)
    textDecoration noneTextDecoration

  fromString ".known-icon-bullet" ? marginRight (10.0 #px)

  fromString ".known-void" ? color gray
  fromString ".known-yes" ? color green
  fromString ".known-no" ? color red
