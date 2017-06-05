module Utilities.Maybe exposing (flatMaybe)

flatMaybe : Maybe (Maybe a) -> Maybe a
flatMaybe maybeSomething =
    case maybeSomething of
          Just something -> something
          Nothing -> Nothing
