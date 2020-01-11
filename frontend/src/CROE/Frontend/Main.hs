module CROE.Frontend.Main
  ( main
  ) where

import           Control.Monad.Reader
import           Language.Javascript.JSaddle (JSM)
import           Reflex.Dom                  hiding (mainWidgetWithHead)
import           Reflex.Dom.Bulma.JSM        (JsmConfig (..), runJsm)
import           Reflex.Dom.Main             (mainWidgetWithHead)
import           System.Environment

import           CROE.Frontend.Env
import           CROE.Frontend.Widget        (primaryWidget)

main :: IO ()
main = do
    [portStr, rootDir] <- getArgs
    let port = read portStr
    runJsm (WarpConfig port rootDir) mainJsm

mainJsm :: JSM ()
mainJsm = mainWidgetWithHead headWidget bodyWidget

headWidget :: MonadWidget t m
           => m ()
headWidget = blank

bodyWidget :: MonadWidget t m
           => m ()
bodyWidget = do
    env <- newEnv
    runReaderT primaryWidget env
