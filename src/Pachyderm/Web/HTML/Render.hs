module Pachyderm.Web.HTML.Render (
    render
) where

import Pachyderm.HList (Empty, (:.)(..), Elem(..))
import Pachyderm.Web.HTML.Interface (HtmlDoc(..), HeadNode(..), Headings(..),
    TextElems(..), Txt(..), FlowContent(..), Sectioning(..), TextSegment, Head, Body, GenBuilder,
    HeadNodeBuilder, BodyBuilder, HRef(..), Lists(..))

import Control.Monad.Reader (Reader, MonadReader, ask, runReader, withReader)
import Data.ByteString (ByteString, intercalate)
import Data.Semigroup ((<>))

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

instance Txt RenderDoc where
    txt = renderWrap

instance HeadNode RenderDoc where
    title content = renderWrap $ "<title>" <> content <> "</title>"
    charset content = renderWrap $ "<meta charset=\""<>content<>"\"></meta>"
    link = undefined

instance Headings RenderDoc where
    h1 content = renderWrap $ "<h1>"<>content<>"</h1>"
    h2 content = renderWrap $ "<h2>"<>content<>"</h2>"
    h3 content = renderWrap $ "<h3>"<>content<>"</h3>"
    h4 content = renderWrap $ "<h4>"<>content<>"</h4>"
    h5 content = renderWrap $ "<h5>"<>content<>"</h5>"

instance FlowContent RenderDoc where
    p contents = do
        cxs <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = intercalate "\n\t" (unWrap <$> cxs)
        renderWrap $ "<p>"<>joined<>"</p>"
    a (HRef link) contents = do
        cxs <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = intercalate "\n\t" (unWrap <$> cxs)
        renderWrap $ "<a href=\""<>link<>"\">"<>joined<>"</a>"
    blockquote = undefined
    div = undefined
    header contents = do
        cxs <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = intercalate "\n\t" (unWrap <$> cxs)
        renderWrap $ "<header>"<>joined<>"</header>"
    footer contents = do
        contents <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = intercalate "\n\t" (unWrap <$> contents)
        renderWrap $ "<footer>"<>joined<>"</footer>"
    hr = undefined
    pre = undefined

instance TextElems RenderDoc where
    i = undefined
    em content = renderWrap $ "<em>"<>content<>"</em>"
    strong = undefined
    small = undefined
    struck = undefined
    quoted = undefined
    code contents = renderWrap $ "<code>"<>contents<>"</code>"
    var = undefined
    time = undefined
    break = undefined
    optionalBreak = undefined

instance Sectioning RenderDoc where
    article contents = do
        contents <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = intercalate "\n\t" (unWrap <$> contents)
        renderWrap $ "<article>"<>joined<>"</article>"
    aside contents = do
        contents <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = intercalate "\n\t" (unWrap <$> contents)
        renderWrap $ "<aside>"<>joined<>"</aside>"
    nav contents = do
        contents <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = intercalate "\n\t" (unWrap <$> contents)
        renderWrap $ "<nav>"<>joined<>"</nav>"

instance Lists RenderDoc where
    ul = undefined
    ol = undefined
    li = undefined

render ::
    RenderDoc a
    -> ByteString
render = unWrap
