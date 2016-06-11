module Parser exposing (..)

import String


pchar : (Char, String) -> Result String (Char, String)
pchar (charToMatch, str) =
  case String.uncons str of
    Nothing ->
      Err "No more input"

    Just (found, rest) ->
      if found == charToMatch then
        Ok (charToMatch, rest)

      else
        Err <|
          "Expecting '" ++ String.fromChar charToMatch
          ++ "'. Got '" ++ String.fromChar found ++ "'"