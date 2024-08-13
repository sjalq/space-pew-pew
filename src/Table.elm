module Table exposing (..)

import Dict exposing (Dict)


type Table a
    = Table Int (Dict Int a)


empty : Table a
empty =
    Table 0 Dict.empty


nextId : Table a -> Int
nextId (Table id _) =
    id


get : Int -> Table a -> Maybe a
get id (Table _ dict) =
    Dict.get id dict


map : (a -> b) -> Table a -> Table b
map fn (Table nextId_ table) =
    Table nextId_ (Dict.map (\_ value -> fn value) table)


filter : (a -> Bool) -> Table a -> Table a
filter fn (Table nextId_ table) =
    Table nextId_ (Dict.filter (\_ value -> fn value) table)


filterMap fn (Table nextId_ table) =
    let
        newDict =
            table
                |> Dict.values
                |> List.filterMap fn
                |> List.map (\v -> ( v.id, v ))
                |> Dict.fromList
    in
    Table nextId_ newDict


insertReturningId record (Table id dict) =
    if record.id == -1 then
        ( id, Table (id + 1) (Dict.insert id { record | id = id } dict) )

    else
        ( record.id, Table (max id (record.id + 1)) (Dict.insert record.id record dict) )


insert record table =
    insertReturningId record table |> Tuple.second


insertMaybe record table =
    record
        |> Maybe.map (\r -> insert r table)
        |> Maybe.withDefault table


remove : Int -> Table a -> Table a
remove id (Table nextId_ dict) =
    Table nextId_ (Dict.remove id dict)


removeMany : List Int -> Table a -> Table a
removeMany ids table =
    List.foldl remove table ids


toList : Table a -> List a
toList (Table _ dict) =
    Dict.values dict


fromList : List { a | id : Int } -> Table { a | id : Int }
fromList records =
    let
        newDict =
            records
                |> List.map (\r -> ( r.id, r ))
                |> Dict.fromList

        maxId : Int
        maxId =
            records
                |> List.map .id
                |> List.maximum
                |> Maybe.map (\id -> id + 1)
                |> Maybe.withDefault 0
    in
    Table maxId newDict


keys : Table a -> List Int
keys (Table _ dict) =
    Dict.keys dict


values : Table a -> List a
values (Table _ dict) =
    Dict.values dict



--union : Table a -> Table a -> Table a


union : Table a -> Table a -> Table a
union (Table nextId1 dict1) (Table nextId2 dict2) =
    let
        maxNextId =
            max nextId1 nextId2
    in
    Table maxNextId (Dict.union dict1 dict2)


size : Table a -> Int
size (Table _ dict) =
    Dict.size dict
