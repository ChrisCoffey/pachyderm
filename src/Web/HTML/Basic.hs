module Web.HTML.Basic  where

import Control.Monad.Reader (Reader, MonadReader, ask, runReader, local)
import Data.ByteString (ByteString, intercalate)
import Data.Monoid ((<>))
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownSymbol, symbolVal)

-- TODO try this out in finally tagless
data Document =
    Document {docHead:: Head, docBody:: Body}

newtype Head =
    Head {headNodes:: [HeadNode]}
newtype HeadNode = HeadNode Node

newtype Body =
    Body {bodyNodes:: [BodyNode]}
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
            h2 "A Heading!!!"
            ]
        )

-- I can accomplish this using a Reader, since that's just a single-cell HList. May want to check parentage
data (:.) a b  = a :. b
data Empty

ls :: Head :. Empty
ls = undefined :. undefined

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

type ParaBuilder rest = GenBuilder Para rest
data Para

-- Is this dumb? A little bit
para ::
    [(ParaBuilder rest) Node]
    -> [Attribute]
    -> BodyBuilder BodyNode
para contentBuilders attribs =
    pure . BodyNode $ Node "p" [runReader c (undefined :. undefined) | c <- contentBuilders] attribs

txt ::
    ByteString
    -> (GenBuilder a rest) Node
txt = pure . Txt

bold :: ByteString -> (ParaBuilder rest) Node
bold contents = pure $ Node "b" [Txt contents] []

h2 ::
    ByteString
    -> BodyBuilder BodyNode
h2 contents = pure . BodyNode $ Node "h2" [Txt contents] []
