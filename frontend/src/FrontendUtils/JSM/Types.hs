module FrontendUtils.JSM.Types
  ( JsmConfig(..)
  ) where

data JsmConfig
  -- | port, root folder
  = WarpConfig Int FilePath
