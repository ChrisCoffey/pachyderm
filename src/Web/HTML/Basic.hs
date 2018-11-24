module Web.HTML.Basic  where

import Control.Monad.Reader (Reader, MonadReader, ask, runReader, withReader)
import Data.ByteString (ByteString, intercalate)
import Data.Monoid (Monoid(..), (<>))
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownSymbol, symbolVal)

class HtmlDoc (repr :: * -> *) where
    hDoc :: repr a -> repr a -> repr a
    hHead :: [HeadNodeBuilder (repr a)] -> repr a
    hBody :: [BodyBuilder (repr a)] -> repr a

class HeadNode' (repr :: * -> *) where
    title' :: ByteString -> HeadNodeBuilder (repr a)

class Headings (repr :: * -> *) where
    h1 :: ByteString -> BodyBuilder (repr a)
    h2 :: ByteString -> BodyBuilder (repr a)
    h3 :: ByteString -> BodyBuilder (repr a)
    h4 :: ByteString -> BodyBuilder (repr a)
    h5 :: ByteString -> BodyBuilder (repr a)

type Known x xs = Elem x xs ~ True

data TextSegment
class TextElems (repr :: * -> *) where
    em :: Known TextSegment rest => ByteString -> Reader rest (repr a)
    strong :: Known TextSegment rest => ByteString -> Reader rest (repr a)
    small :: Known TextSegment rest => ByteString -> Reader rest (repr a)
    struck :: Known TextSegment rest => ByteString -> Reader rest (repr a)
    quoted :: Known TextSegment rest => ByteString -> Reader rest (repr a)
    code :: Known TextSegment rest => ByteString -> Reader rest (repr a)
    var :: Known TextSegment rest => ByteString -> Reader rest (repr a)
    time :: Known TextSegment rest => ByteString -> Reader rest (repr a)
    break :: Known TextSegment rest => Reader rest (repr a)
    optionalBreak :: Known TextSegment rest => Reader rest (repr a)

class Txt' (repr :: * -> *) where
    txt' :: ByteString -> Reader rest (repr a)

class Paragraph (repr :: * -> *) where
    p :: Known Body rest => [Reader (TextSegment :. rest) ( repr a )]  -> Reader rest (repr a)


dummyDoc' :: (HtmlDoc repr, HeadNode' repr, Headings repr,
    TextElems repr, Paragraph repr, Txt' repr) =>
    repr a
dummyDoc' =
    hDoc heading body
    where
        heading = hHead [title' "Test Page II"]
        body = hBody [
            p [
                txt' "Test Doc",
                code "things and such -> great success"
                ],
            h3 "Moar content",
            p [
                txt' "writing stuff"
                ]
            ]

newtype RenderDoc a = RenderDoc {unWrap :: ByteString}

renderWrap ::
    ByteString ->
    Reader xs (RenderDoc a)
renderWrap = pure . RenderDoc

instance HtmlDoc RenderDoc where
    hDoc h b = RenderDoc $
        "<!DOCTYPE html>\n"<>
        "<html lang=\"en\">\n" <>
        unWrap h <>
        unWrap b <>
        "</html>"

    hHead contents = RenderDoc $
        "<head>\n" <>
        intercalate "\n" (unWrap . flip runReader (undefined :. undefined) <$> contents) <>
        "</head>\n"

    hBody contents = RenderDoc $
        "<body>\n" <>
        intercalate "\n" (unWrap . flip runReader (undefined :. undefined) <$> contents) <>
        "</body>\n"

instance Txt' RenderDoc where
    txt' = renderWrap

instance HeadNode' RenderDoc where
    title' content = renderWrap $ "<title>" <> content <> "</title>"

instance Headings RenderDoc where
    h1 content = renderWrap $ "<h1>"<>content<>"</h1>"
    h2 content = renderWrap $ "<h2>"<>content<>"</h2>"
    h3 content = renderWrap $ "<h3>"<>content<>"</h3>"
    h4 content = renderWrap $ "<h4>"<>content<>"</h4>"
    h5 content = renderWrap $ "<h5>"<>content<>"</h5>"

instance Paragraph RenderDoc where
    p contents = do
        contents <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = intercalate "\n\t" (unWrap <$> contents)
        renderWrap $ "<p>"<>joined<>"</p>"

instance TextElems RenderDoc where
    em = undefined
    strong = undefined
    small = undefined
    struck = undefined
    quoted = undefined
    code contents = renderWrap $ "<code>"<>contents<>"</code>"
    var = undefined
    time = undefined
    break = undefined
    optionalBreak = undefined

render ::
    RenderDoc a
    -> ByteString
render = unWrap

-- TODO try this out in finally tagless
data Document =
    Document {docHead:: Head, docBody:: Body}

newtype Head =
    Head {headNodes:: [HeadNode]}
    deriving newtype (Semigroup, Monoid)
newtype HeadNode = HeadNode Node

newtype Body =
    Body {bodyNodes:: [BodyNode]}
    deriving newtype (Semigroup, Monoid)
newtype BodyNode = BodyNode Node

-- TODO make this type level
data Node =
    Node {name :: ByteString, content:: [Node], attributes::[Attribute]}
    | Txt ByteString

data Attribute = Attrib {attribName :: ByteString, attribValue :: ByteString}


renderDoc ::
    Document
    -> ByteString
renderDoc (Document head docBody)=
    "<!DOCTYPE html>\n"<>
    "<html lang=\"en\">\n" <>
    renderHead head <>
    renderBody docBody <>
    "</html>"

renderHead ::
    Head
    -> ByteString
renderHead (Head headNodes) =
    "<head>\n"<>
    intercalate "\n" (renderHeadNode <$> headNodes) <>
    "</head>\n"
    where
        renderHeadNode (HeadNode node) = renderNode node

renderBody ::
    Body
    -> ByteString
renderBody (Body bodyNodes) =
    "<body>\n"<>
    intercalate "\n" (renderBodyNode <$> bodyNodes) <>
    "</body>\n"
    where
        renderBodyNode (BodyNode node) = renderNode node


renderNode ::
    Node
    -> ByteString
renderNode (Txt txt) = txt
renderNode node =
    "<"<>name node<>" "<> attribs <> ">\n" <>
    intercalate "\n\t" (renderNode <$> content node) <>
    "</"<>name node<>">"
    where
        attribs = intercalate "\n\t" $ renderAttribute <$> attributes node

renderAttribute ::
    Attribute
    -> ByteString
renderAttribute (Attrib n v) =
    n <> "=\"" <> v <> "\""

-- The challenge becomes how to construct this nicely and safely
dummyDoc =
    Document
        (mkHead [
            title "Test Page II"
            ])
        ( mkBody [
            para [
                txt "Lots of fun content",
                bold "moar stuff"
                ] [],
            h2' "A Heading!!!"
            ]
        )

data (:.) a b  = a :. b
data Empty

type family Elem needle haystack where
    Elem needle (needle :. rest) = True
    Elem needle (a :. Empty) = False
    Elem needle (a :. rest) = Elem needle rest


type GenBuilder a rest = Reader (a :. rest)
type HeadNodeBuilder = GenBuilder Head Empty
type BodyBuilder = GenBuilder Body Empty

mkHead ::
    [HeadNodeBuilder HeadNode]
    -> Head
mkHead builders = Head [runReader n (undefined :. undefined) | n <- builders]

title ::
    ByteString
    -> HeadNodeBuilder HeadNode
title content = pure . HeadNode $ Node "title" [Txt content] []

mkBody ::
    [Reader (Body :. Empty) BodyNode]
    -> Body
mkBody builders = Body [runReader b (undefined :. undefined) | b <- builders]

txt :: Elem Body rest ~ True =>
    ByteString
    -> (GenBuilder a rest) Node
txt = pure . Txt

h2' ::
    ByteString
    -> BodyBuilder BodyNode
h2' contents = pure . BodyNode $ Node "h2" [Txt contents] []

type ParaBuilder rest = GenBuilder Para rest
data Para

-- Is this dumb? A little bit
para :: Elem Body rest ~ True =>
    [(ParaBuilder rest) Node]
    -> [Attribute]
    -> Reader rest BodyNode
para contentBuilders attribs = do
    contents <- mapM (withReader (\rest -> undefined :.rest )) contentBuilders
    pure . BodyNode $ Node "p" contents attribs

bold :: Elem Para rest ~ True =>
    ByteString
    -> Reader rest Node
bold contents = pure $ Node "b" [Txt contents] []
