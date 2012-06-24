module Static.Templates where

out = putStr

wrap a c b = do
  out a
  b
  out c

doctype = out "<!doctype html>"

html = wrap "<html>" "</html>"

head = wrap "<head>" "</head>"

title = wrap "<title>" "</title>"

body = wrap "<body>" "</body>"

