module Utilities.List exposing (removeWhen, findFirstWhere, allItemsAreEqual,
                                transpose, getAt, chunkify)

removeWhen : (a -> Bool) -> List a -> List a
removeWhen predicate list =
    List.filter (not << predicate) list

findFirstWhere : (a -> Bool) -> List a -> Maybe a
findFirstWhere predicate list =
    case list of
        [] -> Nothing
        first::rest ->
            if predicate first then
                Just first
            else
                findFirstWhere predicate rest

allItemsAreEqual : List a -> Bool
allItemsAreEqual items =
    case items of
         [] -> True
         [first] -> True
         first::rest ->
            case (List.head rest) of
                Nothing -> False
                Just item ->
                   if (first == item) then (allItemsAreEqual rest)
                   else False

transpose : List (List a) -> List (List a)
transpose list =
    doTranspose [] list

doTranspose : List (List a) -> List (List a) -> List (List a)
doTranspose accumulator remainingLines =
    case remainingLines of
        [] ->
            accumulator
        []::linesLeft ->
            doTranspose accumulator linesLeft
        linesLeft ->
            doTranspose
                (accumulator ++ (getAllHeads linesLeft))
                (getAllTails linesLeft)

getAllHeads : List (List a) -> List (List a)
getAllHeads blah =
    [(List.filterMap List.head blah)]

getAllTails : List (List a) -> List (List a)
getAllTails blah =
    (List.filterMap List.tail blah)

getAt : List a -> Int -> Maybe a
getAt xs idx = List.head <| List.drop idx xs

chunkify : Int -> List a -> List (List a)
chunkify chunkSize list =
    case (List.take chunkSize list) of
        [] -> []
        chunk -> chunk :: (chunkify chunkSize (List.drop chunkSize list))

