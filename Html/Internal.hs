module Html.Internal where

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

ul_ :: [Structure] -> Structure
ul_ items =
  Structure (el "ul" (concatMap ui_ items))

ui_ :: Structure -> String
ui_ (Structure a) =
  el "ui_" a

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
  Structure (a <> b)

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
