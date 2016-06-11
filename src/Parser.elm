module Parser exposing (..)

import Result
import String


type Parser a =
  Parser (String -> Result String (a, String))


parse : Parser a -> String -> Result String (a, String)
parse (Parser doParse) input =
  doParse input


andThen : Parser a -> Parser b -> Parser (a, b)
andThen (Parser doFirst) (Parser doRest) =
  Parser <| \str ->
    doFirst str
      `Result.andThen` \(result, rest) ->

    case doRest rest of
      Err msg ->
        Err msg

      Ok (result', rest') ->
        Ok ((result, result'), rest')


pchar : Char -> Parser Char
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