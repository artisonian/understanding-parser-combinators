module Parser exposing (..)

import String


aParser : String -> (Bool, String)
aParser str =
  case String.uncons str of
    Nothing ->
      (False, "")

    Just ('A', rest) ->
      (True, rest)

    _ ->
      (False, str)