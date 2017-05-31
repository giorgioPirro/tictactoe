module Utilities.List exposing (removeWhen, findFirstWhere, allItemsAreEqual)

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
