module Parser exposing
  ( Parser
  , parse
  , andThen, orElse, choice, anyOf
  , bind, map, return, apply, lift2, seq
  , many, many1, opt
  , discardLeft, discardRight, between
  , sepBy, sepBy1
  , pchar, pstring, pint
  )


import Char
import Result
import String


type Parser a =
  Parser (String -> Result String (a, String))


parse : Parser a -> String -> Result String (a, String)
parse (Parser doParse) input =
  doParse input


andThen : Parser a -> Parser b -> Parser (a, b)
andThen firstParser nextParser =
  firstParser `bindP`
    \result ->

  nextParser `bindP`
    \result' ->

  return (result, result')


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


bind : (a -> Parser b) -> Parser a -> Parser b
bind f parser =
  Parser <| \input ->
    parse parser input
      `Result.andThen` \(result, rest) ->

    parse (f result) rest


bindP : Parser a -> (a -> Parser b) -> Parser b
bindP = flip bind


map : (a -> b) -> Parser a -> Parser b
map f =
  bind (f >> return)


return : a -> Parser a
return thing =
  Parser <| \input ->
    Ok (thing, input)


apply : (Parser (a -> b)) -> Parser a -> Parser b
apply fP thingP =
  fP `bindP`
    \f ->

  thingP `bindP`
    \x ->

  return (f x)


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


zeroOrMore : Parser a -> String -> (List a, String)
zeroOrMore parser input =
  case (parse parser input) of
    Err _ ->
      ([], input)

    Ok (result, rest) ->
      let
        (result', rest') = zeroOrMore parser rest

      in
        (result :: result', rest')


many : Parser a -> Parser (List a)
many parser =
  Parser <| \input ->
    Ok <| zeroOrMore parser input


many1 : Parser a -> Parser (List a)
many1 parser =
  parser `bindP`
    \head ->

  many parser `bindP`
    \tail ->

  return (head :: tail)


opt : Parser a -> Parser (Maybe a)
opt parser =
  let
    some =
      map Just parser

    none =
      return Nothing

  in
    some `orElse` none


discardLeft : Parser a -> Parser b -> Parser b
discardLeft left right =
  left `andThen` right
    |> map (\(a, b) -> b)


discardRight : Parser a -> Parser b -> Parser a
discardRight left right =
  left `andThen` right
    |> map (\(a, b) -> a)


between : Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 =
  p1 `discardLeft` p2 `discardRight` p3


sepBy1 : Parser a -> Parser b -> Parser (List a)
sepBy1 parser sep =
  let
    sepThenP = sep `discardLeft` parser

  in
    map (\(p, list) -> p :: list) <|
      parser `andThen` many sepThenP


sepBy : Parser a -> Parser b -> Parser (List a)
sepBy parser sep =
  sepBy1 parser sep `orElse` return []


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


pint : Parser Int
pint =
  let
    toInt (sign, chars) =
      let
        n = chars |> String.fromList |> String.toInt |> Result.withDefault 0

      in
        case sign of
          Just _ ->
            negate n

          Nothing ->
            n

    digit =
      anyOf pchar <| List.map Char.fromCode [48..57]

    digits =
      many1 digit

  in
    opt (pchar '-') `andThen` digits
      |> map toInt