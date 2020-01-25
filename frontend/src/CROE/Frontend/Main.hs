module CROE.Frontend.Main
  ( main
  ) where

import           Language.Javascript.JSaddle (JSM)
import           Reflex.Dom                  hiding (mainWidgetWithHead)
import           Reflex.Dom.Bulma.JSM        (JsmConfig (..), runJsm)
import           Reflex.Dom.Main             (mainWidgetWithHead)
import           System.Environment

import           CROE.Frontend.Env
import           CROE.Frontend.Widget        (primaryWidget)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [portStr, rootDir] -> mainWithArgs portStr rootDir
      _                  -> mainWithArgs "8080" "static"

mainWithArgs :: String
             -> FilePath
             -> IO ()
mainWithArgs portStr rootDir = do
    let port = read portStr
    runJsm (WarpConfig port rootDir) mainJsm

mainJsm :: JSM ()
mainJsm = mainWidgetWithHead headWidget bodyWidget

headWidget :: MonadWidget t m
           => m ()
headWidget = do
    elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
    elAttr "link" ("rel" =: "icon" <> "type" =: "image/png" <> "href" =: "/favicon.png") blank
    elAttr "link" ("rel" =: "stylesheet" <> "href" =: "/all.css") blank
    elAttr "script" ("defer" =: "" <> "src" =: "https://use.fontawesome.com/releases/v5.3.1/js/all.js") blank

bodyWidget :: MonadWidget t m
           => m ()
bodyWidget = do
    env <- newEnv
    primaryWidget env
