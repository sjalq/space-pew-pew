module Evergreen.V7.Table exposing (..)

import Dict


type Table a
    = Table Int (Dict.Dict Int a)
