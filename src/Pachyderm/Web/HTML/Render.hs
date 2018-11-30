module Pachyderm.Web.HTML.Render (
    render
) where

import Pachyderm.HList (Empty, (:.)(..), Elem(..))
import Pachyderm.Web.HTML.Interface (HtmlDoc(..), HeadNode(..), Headings(..),
    TextElems(..), Txt(..), FlowContent(..), Sectioning(..), TextSegment, Head, Body, GenBuilder,
    HeadNodeBuilder, BodyBuilder, HRef(..), Lists(..), LinkType(..))

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
    link (HRef path) (LinkType tpe) =
        renderWrap $ "<link type=\""<>tpe<>"\" href=\""<>path<>"\">"

instance Headings RenderDoc where
    h1 content = renderWrap $ "<h1>"<>content<>"</h1>"
    h2 content = renderWrap $ "<h2>"<>content<>"</h2>"
    h3 content = renderWrap $ "<h3>"<>content<>"</h3>"
    h4 content = renderWrap $ "<h4>"<>content<>"</h4>"
    h5 content = renderWrap $ "<h5>"<>content<>"</h5>"

renderInExtendedContext node extension contents = do
    cxs <- mapM (withReader (\rest -> extension :.rest )) contents
    let joined = intercalate "\n\t" (unWrap <$> cxs)
    renderWrap $ "<p>"<>joined<>"</p>"

renderInSameContext node cxs = do
    contents <- sequence cxs
    let joined = intercalate "\n\t" (unWrap <$> contents)
    renderWrap $ "<"<>node<>">"<>joined<>"</"<>node<>">"


instance FlowContent RenderDoc where
    p = renderInExtendedContext "p" undefined
    a (HRef link) contents = do
        cxs <- mapM (withReader (\rest -> undefined :.rest )) contents
        let joined = intercalate "\n\t" (unWrap <$> cxs)
        renderWrap $ "<a href=\""<>link<>"\">"<>joined<>"</a>"
    blockquote = renderInSameContext "blockquote"
    div = renderInSameContext "div"
    header = renderInExtendedContext "header" undefined
    footer = renderInExtendedContext "header" undefined
    hr = renderWrap "</hr>"
    pre = renderInExtendedContext "pre" undefined

instance TextElems RenderDoc where
    i content = renderWrap $ "<i>"<>content<>"</i>"
    em content = renderWrap $ "<em>"<>content<>"</em>"
    strong content = renderWrap $ "<strong>"<>content<>"</strong>"
    small content = renderWrap $ "<small>"<>content<>"</small>"
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
    ul = renderInExtendedContext "ul" undefined
    --TODO factor all of this copy+paste out
    ol = renderInExtendedContext "ol" undefined
    li = renderInSameContext "li"

render ::
    RenderDoc a
    -> ByteString
render = unWrap
