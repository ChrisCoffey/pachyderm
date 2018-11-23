module Web.HTML.Basic (
) where

import Data.ByteString (ByteString)
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
data Node = Node {name :: ByteString, content:: [Node], attributes::[Attribute]}

data Attribute = Attrib {attribName :: ByteString, attribValue :: ByteString}


renderDoc ::
    Document
    -> ByteString
renderDoc doc =
    "<!DOCTYPE html>\n"<>
    "<html lang=\"en\">\n" <>
    "\t<head>\n"<>
    "\t"

