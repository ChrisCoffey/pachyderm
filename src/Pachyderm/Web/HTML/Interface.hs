module Pachyderm.Web.HTML.Interface (
    HtmlDoc(..),
    HeadNode(..),
    Headings(..),
    TextElems(..),
    Txt(..),
    FlowContent(..),
    Sectioning(..),
    Lists(..),

    -- | Newtypes
    HRef(..),
    LinkType(..),

    -- | Uninhabited markers
    TextSegment,
    Head,
    Body,
    Header,
    Footer,
    List,

    -- | Builder helpers
    GenBuilder,
    HeadNodeBuilder,
    BodyBuilder
    ) where

import Pachyderm.HList (Empty, Known, (:.)(..))

import Control.Monad.Reader (Reader)
import Data.ByteString (ByteString)
import Data.Type.Bool

type GenBuilder a rest = Reader (a :. rest)
type HeadNodeBuilder = GenBuilder Head Empty
type BodyBuilder = GenBuilder Body Empty

class HtmlDoc (repr :: * -> *) where
    hDoc :: repr a -> repr a -> repr a
    hHead :: [HeadNodeBuilder (repr a)] -> repr a
    hBody :: [BodyBuilder (repr a)] -> repr a

data Head
data Body

newtype HRef = HRef ByteString
newtype LinkType = LinkType ByteString

class HeadNode (repr :: * -> *) where
    title :: ByteString -> HeadNodeBuilder (repr a)
    charset :: ByteString -> HeadNodeBuilder (repr a)
    link :: HRef -> LinkType -> HeadNodeBuilder (repr a)

class Headings repr where
    h1 :: (AcceptsFlow rest ~ True) => ByteString -> Reader rest (repr a)
    h2 :: (AcceptsFlow rest ~ True) => ByteString -> Reader rest (repr a)
    h3 :: (AcceptsFlow rest ~ True) => ByteString -> Reader rest (repr a)
    h4 :: (AcceptsFlow rest ~ True) => ByteString -> Reader rest (repr a)
    h5 :: (AcceptsFlow rest ~ True) => ByteString -> Reader rest (repr a)


data TextSegment
class TextElems (repr :: * -> *) where
    em :: Known TextSegment rest => ByteString -> Reader rest (repr a)
    i :: Known TextSegment rest => ByteString -> Reader rest (repr a)
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

data Article
data Nav
data Aside
class Sectioning repr where
    article :: Known Body rest => [Reader (Article :. rest) ( repr a )] -> Reader rest (repr a)
    aside :: (Known Body rest) => [Reader (Aside :. rest) (repr a)] -> Reader rest (repr a)
    nav :: (Known Body rest) => [Reader (Nav :. rest) (repr a)] -> Reader rest (repr a)

type family SectioningContent a where
    SectioningContent Article = True
    SectioningContent Nav = True
    SectioningContent Aside = True
    SectioningContent _ = False

type family Same a b where
    Same a a = True
    Same a b = False

type family AcceptsFlow xs where
    AcceptsFlow (a :. Empty) = (SectioningContent a || Same a Body)
    AcceptsFlow (a :. rest) = (SectioningContent a || Same a Body) || AcceptsFlow rest


data Header
data Footer
class FlowContent repr where
    a :: (AcceptsFlow rest ~ True) => HRef -> [Reader (TextSegment :. rest) (repr a)] -> Reader rest (repr a)
    p :: (AcceptsFlow rest ~ True) => [Reader (TextSegment :. rest) ( repr a )]  -> Reader rest (repr a)
    blockquote :: (AcceptsFlow rest ~ True) => [Reader rest ( repr a )]  -> Reader rest (repr a)
    div :: (AcceptsFlow rest ~ True) => [Reader rest ( repr a )]  -> Reader rest (repr a)
    header :: (AcceptsFlow rest ~ True) => [Reader (Header :. rest) ( repr a )]  -> Reader rest (repr a)
    footer :: (AcceptsFlow rest ~ True) => [Reader (Footer :. rest) ( repr a )]  -> Reader rest (repr a)
    hr :: (AcceptsFlow rest ~ True) => Reader rest (repr a)
    pre :: (AcceptsFlow rest ~ True) => [Reader (TextSegment :. rest) ( repr a )]  -> Reader rest (repr a)


data List
class Lists repr where
    ul :: ( AcceptsFlow rest ~ True ) => [Reader (List :. rest) ( repr a )] -> Reader rest (repr a)
    ol :: ( AcceptsFlow rest ~ True ) => [Reader (List :. rest) ( repr a )] -> Reader rest (repr a)
    li :: (AcceptsFlow rest ~ True) => [Reader (List :. rest) ( repr a )] -> Reader (List :. rest) (repr a)
