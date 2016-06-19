module Parser.Infix exposing (..)


import Parser exposing
  ( Parser
  , bind, map, apply
  , andThen, orElse
  , takeLeft, takeRight
  )


(>>=) : Parser a -> (a -> Parser b) -> Parser b
(>>=) = flip bind


(<!>) : (a -> b) -> Parser a -> Parser b
(<!>) = map


(|>>) : Parser a -> (a -> b) -> Parser b
(|>>) = flip map


(<*>) : Parser (a -> b) -> Parser a -> Parser b
(<*>) = apply


(.>>.) : Parser a -> Parser b -> Parser (a, b)
(.>>.) = andThen


(<|>) : Parser a -> Parser a -> Parser a
(<|>) = orElse


(.>>) : Parser a -> Parser b -> Parser a
(.>>) = takeLeft


(>>.) : Parser a -> Parser b -> Parser b
(>>.) = takeRight