module Parser exposing (..)

import String


pchar : (Char, String) -> (String, String)
pchar (charToMatch, str) =
  case String.uncons str of
    Nothing ->
      ("No more input", "")

    Just (found, rest) ->
      if found == charToMatch then
        ("Found " ++ String.fromChar charToMatch, rest)

      else
        ( "Expecting '" ++ String.fromChar charToMatch
          ++ "'. Got '" ++ String.fromChar found ++ "'"
        , str
        )