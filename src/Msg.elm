module Msg exposing (Msg(..))

import Board exposing (Position, Size)
import GameGenerator exposing (GameType)

type Msg
    = MakeMove Position
    | NewGame Size GameType
