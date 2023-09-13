module Html.Internal where

import GHC.Natural

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title =
  String

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

h_ :: Natural -> String -> Structure
h_ n =
  Structure . el ("h" <> show n) . escape

empty_ :: Structure
empty_ = Structure ""

ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

instance Semigroup Structure where
  (<>) :: Structure -> Structure -> Structure
  (<>) s1 s2 =
    Structure (getStructureString s1 <> getStructureString s2)

instance Monoid Structure where
  mempty = empty_

render :: Html -> String
render html =
  case html of
    Html str -> str

getStructureString :: Structure -> String
getStructureString struct =
  case struct of
    Structure str -> str

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quote;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar
