module Pachyderm.Web.HTML.Interface (
    HtmlDoc(..),
    HeadNode(..),
    Headings(..),
    TextElems(..),
    Txt(..),
    Paragraph(..),

    -- | Uninhabited markers
    TextSegment,
    Head,
    Body,

    -- | Builder helpers
    GenBuilder,
    HeadNodeBuilder,
    BodyBuilder
    ) where

import Pachyderm.HList (Empty, Known, (:.)(..))

import Control.Monad.Reader (Reader)
import Data.ByteString (ByteString)

type GenBuilder a rest = Reader (a :. rest)
type HeadNodeBuilder = GenBuilder Head Empty
type BodyBuilder = GenBuilder Body Empty

class HtmlDoc (repr :: * -> *) where
    hDoc :: repr a -> repr a -> repr a
    hHead :: [HeadNodeBuilder (repr a)] -> repr a
    hBody :: [BodyBuilder (repr a)] -> repr a

data Head
data Body

class HeadNode (repr :: * -> *) where
    title :: ByteString -> HeadNodeBuilder (repr a)
    charset :: ByteString -> HeadNodeBuilder (repr a)

class Headings (repr :: * -> *) where
    h1 :: ByteString -> BodyBuilder (repr a)
    h2 :: ByteString -> BodyBuilder (repr a)
    h3 :: ByteString -> BodyBuilder (repr a)
    h4 :: ByteString -> BodyBuilder (repr a)
    h5 :: ByteString -> BodyBuilder (repr a)


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

class Txt (repr :: * -> *) where
    txt :: ByteString -> Reader rest (repr a)

class Paragraph (repr :: * -> *) where
    p :: Known Body rest => [Reader (TextSegment :. rest) ( repr a )]  -> Reader rest (repr a)
