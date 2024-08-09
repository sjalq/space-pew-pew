module Evergreen.V5.Table exposing (..)

import Dict


type Table a
    = Table Int (Dict.Dict Int a)
