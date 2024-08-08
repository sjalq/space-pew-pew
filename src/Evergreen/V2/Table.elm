module Evergreen.V2.Table exposing (..)

import Dict


type Table a
    = Table Int (Dict.Dict Int a)
