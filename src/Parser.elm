module Parser exposing
  ( Parser
  , parse
  , andThen, orElse, choice, anyOf
  , map, return, apply, lift2, seq
  , pchar, pstring
  )

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


orElse : Parser a -> Parser a -> Parser a
orElse (Parser tryFirst) (Parser tryNext) =
  Parser <| \str ->
    case tryFirst str of
      Ok result ->
        Ok result

      Err _ ->
        tryNext str


choice : List (Parser a) -> Parser a
choice parsers =
  Parser (oneOf parsers)


oneOf : List (Parser a) -> String -> Result String (a, String)
oneOf parsers str =
  case parsers of
    [] ->
      Err "All parsers failed"

    Parser tryFirst :: others ->
      case tryFirst str of
        Err _ ->
          oneOf others str

        Ok result ->
          Ok result


anyOf : (a -> Parser a) -> List a -> Parser a
anyOf parser inputs =
  inputs
    |> List.map parser
    |> choice


map : (a -> b) -> Parser a -> Parser b
map f parser =
  Parser <| \input ->
    case (parse parser input) of
      Ok (result, rest) ->
        Ok (f result, rest)

      Err msg ->
        Err msg


return : a -> Parser a
return thing =
  Parser <| \input ->
    Ok (thing, input)


apply : (Parser (a -> b)) -> Parser a -> Parser b
apply fP thingP =
  fP `andThen` thingP |> map (\(f, x) -> f x)


lift2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 f xP yP =
  return f `apply` xP `apply` yP


seq : List (Parser a) -> Parser (List a)
seq list =
  let
    consP = lift2 (::)

  in
    case list of
      [] ->
        return []
      x :: xs ->
        consP x (seq xs)


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


pstring : String -> Parser String
pstring str =
  str
    |> String.toList
    |> List.map pchar
    |> seq
    |> map String.fromList