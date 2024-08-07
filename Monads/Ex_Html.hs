
data Html a = Html [String] a

mkHtml :: Html a -> String
mkHtml (Html acc _) = concat acc

instance Functor Html where
  fmap f (Html acc a) = Html acc (f a)

instance Applicative Html where
  pure = Html []
  (Html acc1 f) <*> (Html acc2 a) = Html (acc1 ++ acc2) (f a)

instance Monad Html where
  return = pure
  (Html acc1 a) >>= f =
    let Html acc2 b = f a
    in Html (acc1 ++ acc2) b

raw :: String -> Html ()
raw s = Html [s] ()

noContent :: Html ()
noContent = raw ""

--------------------------------------------------------------------

type Attr = (String, String)
data Node = Node String [Attr]

tag :: String -> Node
tag s = Node s []

(<+>) :: Node -> Attr -> Node
(Node s as) <+> a = Node s (a:as)

(<++>) :: Node -> [Attr] -> Node
(Node s as) <++> a = Node s (a++as)

(-<) :: Node -> Html a -> Html a
n -< (Html acc b)
    = Html (openNode n : acc ++ [closeNode n]) b
    where
    openNode (Node s as) = "<" ++ s ++ " " ++ concatMap showAttr as ++ ">"
    closeNode (Node s _) = "</" ++ s ++ ">"
    showAttr (k, v) = k ++ "=\"" ++ v ++ "\""

infixr 0 -<

-----------------------------------------------------------------

html = tag "html"
head' = tag "head"
title = tag "title"
body = tag "body"
h1 string = tag "h1" -< raw string
p string = tag "p" -< raw string
a = tag "a"
div = tag "div"
span = tag "span"

ul = tag "ul"
ol = tag "ol"
li = tag "li"



br = raw "<br>"
hr = raw "<hr>"

--------------------------------------------------------------------

style :: [Attr] -> [Attr]
style [] = []
style xs = [("style", concatMap (\(k, v) -> k ++ ":" ++ v ++ ";") xs)]


bootstrap :: Html ()
bootstrap =
    tag "link"
        <+> ("href", "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css")
        <+> ("rel","stylesheet")
        <+> ("integrity","sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH")
        <+> ("crossorigin","anonymous")
        -< noContent

test :: Html ()
test =
      html -< do
        head' -< do
          title -< raw "Hello"
          bootstrap
        body <++> style [("color", "blue")]
             <+> ("class", "main") -< do
          h1 "Hello"
          p "World"
          hr
          a <+> ("href", "http://www.google.com")
            <+> ("class", "btn btn-primary") -< raw "Google"
          ul -< do
              li -< raw "One"
              li -< raw "Two"
              li -< raw "Three"


main :: IO ()
main = writeFile "index.html" $ mkHtml test