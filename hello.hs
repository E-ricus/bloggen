main :: IO ()
main =
  putStrLn myHtml

makeHtml :: String -> String -> String
makeHtml title body = html_ (head_ (title_ title) <> body_ body)

myHtml :: String
myHtml = makeHtml "My page title" "My page content"

body_ :: String -> String
body_ content = "<body>" <> content <> "</body>"

head_ :: String -> String
head_ title = "<head>" <> title <> "</head>"

title_ :: String -> String
title_ tl = "<title>" <> tl <> "</title>"

html_ :: String -> String
html_ content = "<html>" <> content <> "</html>"
