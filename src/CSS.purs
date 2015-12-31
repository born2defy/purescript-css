module CSS where

import    Prelude               hiding (top, bottom)
import    Control.Monad.Eff
import    Control.Monad.Eff.Exception
import    Control.Monad.Eff.Console hiding (error)
import    CSS.Media       as M
import    Data.Maybe
import    Data.Tuple.Nested
import    DOM
import    Data.NonEmpty   as NEL
import    Node.FS
import    CSS.CSSCompiled


main :: forall eff. Eff (dom::DOM, fs::FS, err::EXCEPTION, console::CONSOLE | eff) Unit
main = do
  renderAndCompile style

blue1 :: Color
blue1 = rgb 51 136 204

blue2 :: Color
blue2 = rgb 238 238 255

backgroundGradient :: forall a. Angle a -> CSS
backgroundGradient a = backgroundImage $ linearGradient a (ColorPoint white (pct 0.0)) [] (ColorPoint blue2 (pct 100.0))

shake :: (Number -> Number) -> CSS
shake f = transforms [translate (px (f 3.0)) nil, rotate (deg (f 2.0))]

style :: CSS
style = do
  fontFace $ do
    fontFaceFamily $ fromString "Lato"
    fontWeight $ weight 300.0
    fontFaceSrc $ FontFaceSrcLocal "Lato Light" NEL.:|
                [ FontFaceSrcLocal "Lato-Light"
                , FontFaceSrcUrl "http://fonts.gstatic.com/s/lato/v11/EsvMC5un3kjyUhB9ZEPPwg.woff2" (Just WOFF2)
                ]

  keyframes "buzz-button" $ tuple2 50.0 (shake id) NEL.:| [tuple2 100.0 (shake negate)]

  query M.screen (NEL.singleton <<< M.maxWidth $ px 768.0) $
    h1 ? do
        fontSize (em 2.0)

  html ? height (pct 100.0)
  body ? do
    fontFamily ["Lato"] (NEL.singleton sansSerif)
    sym padding nil
    sym margin nil
    backgroundGradient (deg 0.0)
  (h1 ** a) ? do
    display block
    color blue1
    textDecoration noneTextDecoration
    fontWeight $ weight 100.0
    sym padding (em 0.5)
  h1 ? do
    fontSize (em 3.0)
    position absolute
    left (pct 50.0)
    top (pct 50.0)
    backgroundGradient (deg 180.0)
    border solid (px 1.0) blue1
    sym borderRadius (em 0.25)
  (h1 ## hover) ?
    animation (fromString "buzz-button") (sec 0.15) linear (sec 0.0) infinite normalAnimationDirection forwards
