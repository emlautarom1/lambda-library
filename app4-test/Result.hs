module Result where

data Result value
  = Ok value
  | Err String
  deriving (Show, Eq)

instance Functor Result where
  fmap func result =
    case result of
      Ok value -> Ok (func value)
      Err e -> Err e

instance Applicative Result where
  pure = Ok

  (<*>) r1 r2 =
    case (r1, r2) of
      (Ok func, Ok a) -> Ok (func a)
      (Err e, _) -> Err e
      (Ok _, Err e) -> Err e

instance Monad Result where
  (>>=) result func =
    case result of
      Ok value -> func value
      Err e -> Err e

instance MonadFail Result where
  fail = Err
