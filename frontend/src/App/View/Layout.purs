module App.View.Layout where

import App.Events (Event)
import App.Routes (Route(NotFound, Home))
import App.State (State(..))
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import CSS (CSS, backgroundColor, borderRadius, color, display, fontSize, fromString, inlineBlock, key, marginLeft, marginRight, marginTop, maxWidth, padding, pct, px, value, (?))
import CSS.Border (border, solid)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (center, textAlign)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
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
  let green = rgb 14 196 172
      blue = rgb 14 154 196
      white = rgb 250 250 250

  fromString "body" ? do
    backgroundColor (rgb 0 20 30)
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")
    color white

  fromString "h1" ? do
    fontSize (48.0 #px)
    marginTop (48.0 #px)
    textTransform uppercase
    letterSpacing (6.0 #px)
    textAlign center

  fromString ".container" ? do
    maxWidth (80.0 #pct)

  fromString "a" ? do
    display inlineBlock
    borderRadius (2.0 #px) (2.0 #px) (2.0 #px) (2.0 #px)
    padding (6.0 #px) (6.0 #px) (6.0 #px) (6.0 #px)
    textDecoration noneTextDecoration

  fromString ".guide" ? do
    border solid (2.0 #px) green
    color green
    marginRight (10.0 #px)

  fromString ".guide:hover" ? do
    backgroundColor green
    color white

  fromString ".github" ? do
    border solid (2.0 #px) blue
    color blue
    marginLeft (10.0 #px)

  fromString ".github:hover" ? do
    backgroundColor blue
    color white
