{-# LANGUAGE CPP #-}

module FrontendUtils.JSM.GHCJS where

-- make hie ignore this module
#ifdef ghcjs_HOST_OS

import           FrontendUtils.JSM.Types
import           Language.Javascript.JSaddle.Types (JSM)

runJsm :: JsmConfig -> JSM () -> IO ()
runJsm _ = id

#endif
