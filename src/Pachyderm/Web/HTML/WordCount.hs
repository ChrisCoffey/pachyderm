module Pachyderm.Web.HTML.WordCount (
    wordCount
) where

import Pachyderm.HList (Empty, (:.)(..), Elem(..))
import Pachyderm.Web.HTML.Interface (HtmlDoc(..), HeadNode(..), Headings(..),
    TextElems(..), Txt(..), Paragraph(..), TextSegment, Head, Body, GenBuilder,
    HeadNodeBuilder, BodyBuilder)

import qualified Data.ByteString as BS
import Data.Char (ord)
import Control.Monad.Reader (Reader, MonadReader, ask, runReader, withReader)

newtype WCDoc a = WCDoc {unWrap :: Int}

wcWrap ::
    Int ->
    Reader xs (WCDoc a)
wcWrap = pure . WCDoc

wc ::
    BS.ByteString
    -> Int
wc bs
    | BS.null bs = 0
    | otherwise = (+ 1) $ BS.count (fromIntegral $ ord ' ') bs

instance HtmlDoc WCDoc where
    hDoc _ b = WCDoc $ unWrap b
    hHead _ = WCDoc 0

    hBody contents = WCDoc $
        sum (unWrap . flip runReader (undefined :. undefined) <$> contents)

instance Txt WCDoc where
    txt = wcWrap . wc

instance HeadNode WCDoc where
    title content = wcWrap 0
    charset content = wcWrap 0

instance Headings WCDoc where
    h1 = wcWrap . wc
    h2 = wcWrap . wc
    h3 = wcWrap . wc
    h4 = wcWrap . wc
    h5 = wcWrap . wc

instance Paragraph WCDoc where
    p contents = do
        contents <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = sum (unWrap <$> contents)
        wcWrap joined

instance TextElems WCDoc where
    em = undefined
    strong = undefined
    small = undefined
    struck = undefined
    quoted = undefined
    code = wcWrap . wc
    var = undefined
    time = undefined
    break = undefined
    optionalBreak = undefined

wordCount ::
    WCDoc a
    -> Int
wordCount = unWrap
