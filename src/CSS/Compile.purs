module CSS.Compile (
    addStyleSheet
  , compile
  , renderAndInsertStyle, renderAndCompile
  ) where

import  Prelude
import  Control.Monad.Eff.Console hiding (error)
import  Control.Monad.Eff
import  Control.Monad.Eff.Exception
import  Node.FS
import  Node.FS.Sync as FS
import  Node.Encoding
import  DOM
import  Control.Apply                    ((*>))
import  CSS.RenderForCompilation         (renderStyleSheet)
import  CSS.Render                       as Render
import  CSS.Stylesheet                    (CSS())

foreign import addStyleSheet :: forall eff. String -> Eff (dom::DOM | eff) Unit

renderAndInsertStyle :: forall eff. CSS -> Eff (dom::DOM | eff) Unit
renderAndInsertStyle = addStyleSheet <<< Render.renderStyleSheet

renderAndCompile :: forall eff. CSS -> Eff (fs::FS, err::EXCEPTION, console::CONSOLE | eff) Unit
renderAndCompile = compile <<< renderStyleSheet

compile :: forall eff. String -> Eff (fs::FS, err::EXCEPTION, console::CONSOLE | eff) Unit
compile css = do
  log "Compiling CSS file into .\\css\\Main.css"
  let cssPath = ".\\css"
  cssExists <- FS.exists cssPath
  if cssExists then mkCSSFile cssPath else mkCSSDir cssPath >>= mkCSSFile
    where
    mkCSSFile dirPath =
      let cssFilePath = "\\Main.css"
      in  FS.writeTextFile UTF8 (dirPath <> cssFilePath) css

    mkCSSDir dirPath = FS.mkdir dirPath *> pure dirPath
