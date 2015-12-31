module Test.Main where

import Prelude
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console hiding (error)
import CSS.Border
import CSS.Color
import CSS.Display
import CSS.Elements
import CSS.Font
import CSS.Render
import CSS.Selector
import CSS.Size
import CSS.String
import CSS.Stylesheet
import Data.Maybe
import Data.These
import DOM
import Node.FS

import Test.CompileTest as CompileTest


main ::  forall eff. Eff (dom::DOM, fs::FS, err::EXCEPTION, console::CONSOLE | eff) Unit
main = do
  renderedInline example1 `assertEqual` Just "color: rgb(255, 0, 0); display: block"
  renderedInline example2 `assertEqual` Just "display: inline-block"
  renderedInline example3 `assertEqual` Just "border: dashed 2.0px rgb(0, 128, 0)"
  selector (Selector (Refinement [Id "test"]) Star) `assertEqual` "#test"
  selector (fromString "#test") `assertEqual` "#test"
  renderedSheet example4 `assertEqual` Just "body { color: rgb(0, 128, 0) }\n#world { display: block }\n"

  CompileTest.main


example1 :: Rendered
example1 = render do
  color red
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

example3 :: Rendered
example3 = render do
  border dashed (px 2.0) green

example4 :: Rendered
example4 = render do
  body ? do
    color green
  fromString "#world" ? do
    display block

assertEqual :: forall a eff. (Eq a, Show a) => a -> a -> Eff (err :: EXCEPTION | eff) Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y
