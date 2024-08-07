module Structures exposing (..)

import Dict exposing (Dict)


fromIdList records =
    records
        |> List.map (\r -> ( r.id, r ))
        |> Dict.fromList


insertMaybe record dict =
    record
        |> Maybe.map (\r -> Dict.insert r.id r dict)
        |> Maybe.withDefault dict
