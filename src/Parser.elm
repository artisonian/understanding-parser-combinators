module Parser exposing (..)

import String


type Parser a =
  Parser (String -> Result String a)


parse : Parser a -> String -> Result String a
parse (Parser doParse) input =
  doParse input


pchar : Char -> Parser (Char, String)
pchar charToMatch =
  Parser <| \str ->
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