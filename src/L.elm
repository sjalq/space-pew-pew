module L exposing (..)

import Lamdera exposing (ClientId)
import Types exposing (ToBackend, ToFrontend)


{--
    This is a wrapper around Lamdera.sendToBackend and Lamdera.sendToFrontend
    to help VS Code suggest new msg types.
--}

sendToBackend : ToBackend -> Cmd msg
sendToBackend =
    Lamdera.sendToBackend


sendToFrontend : ClientId -> ToFrontend -> Cmd msg
sendToFrontend =
    Lamdera.sendToFrontend

broadcast : ToFrontend -> Cmd msg
broadcast =
    Lamdera.broadcast