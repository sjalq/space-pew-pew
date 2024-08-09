module Evergreen.V4.Table exposing (..)

import Dict


type Table a
    = Table Int (Dict.Dict Int a)
