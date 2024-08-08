module Evergreen.V1.Table exposing (..)

import Dict


type Table a
    = Table Int (Dict.Dict Int a)
