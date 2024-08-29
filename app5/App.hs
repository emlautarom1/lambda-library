{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module App (Env (..), App, runApp) where

import Books
import Control.Monad.Reader

newtype Env = Env {db :: BookDB}

newtype App a = App (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runApp :: Env -> App a -> IO a
runApp env (App f) =
  runReaderT f env