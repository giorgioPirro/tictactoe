module Utilities.Maybe exposing (flatMaybe)

flatMaybe : Maybe (Maybe a) -> Maybe a
flatMaybe mx =
    case mx of
          Just x -> x
          Nothing -> Nothing
