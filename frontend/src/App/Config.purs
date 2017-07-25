module App.Config where

type Config =
  { title :: String
  , publicPath :: String
  , apiBase :: String
  }

foreign import config :: Config
