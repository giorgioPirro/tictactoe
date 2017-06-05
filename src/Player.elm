module Player exposing (Player(..), extractMark)

import Board exposing (Mark)

type Player
    = Human Mark
    | Computer Mark

extractMark : Player -> Mark
extractMark player =
    case player of
        Human mark -> mark
        Computer mark -> mark
