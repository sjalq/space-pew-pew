module L exposing (..)

import Lamdera exposing (ClientId)
import Task
import Types exposing (ToBackend, ToFrontend)
import Time



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


performNow : msg -> Cmd msg
performNow msg_ =
    Task.perform identity (Task.succeed msg_)


performWithTime : (Time.Posix -> a) -> Cmd a
performWithTime f =
    Time.now |> Task.perform f
