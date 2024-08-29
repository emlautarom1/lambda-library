{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Task
  ( Task,
    DoAnythingHandler,
    doAnythingHandler,
    doAnything,
    perform,
  )
where

-- In this module, hiding the representation of the types becomes crucial.

-- A task is essentially a wrapped IO. Again, we need to perform IO at the end
-- of the day.

newtype Task a = Task {run :: IO a}
  -- Thanks to GeneralisedNewtypeDeriving we can derive Monad and MonadFail from the wrapped IO, but otherwise we can manually define it.
  deriving (Functor, Applicative, Monad, MonadFail)

-- The `DoAnythingHandler` seems useless, the key part is that you can ONLY
-- get a `DoAnythingHandler` value from the `doAnythingHandler` function which is an IO.
--
-- This means that you will NOT be able to get it from a Task. You main function will
-- get and use this `DoAnythingHandler` value to create the Handlers we will pass around.

data DoAnythingHandler = DoAnythingHandler

doAnythingHandler :: IO DoAnythingHandler
doAnythingHandler = pure DoAnythingHandler

doAnything :: DoAnythingHandler -> IO a -> Task a
doAnything _ io = Task {run = io}

perform :: Task a -> IO a
perform = run
