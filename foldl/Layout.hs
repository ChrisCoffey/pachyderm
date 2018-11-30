module Layout (
    blogLayout
) where

import Pachyderm.Web.HTML.Interface
import Pachyderm.HList

import Control.Monad.Reader (Reader)
import Data.ByteString (ByteString)

blogLayout :: (HtmlDoc repr, HeadNode repr, Headings repr,
    TextElems repr, FlowContent repr, Txt repr, Sectioning repr,
    Lists repr) =>
    BodyBuilder (repr a)
    -> repr a
blogLayout content =
    hDoc heading body
    where
        heading = hHead [title "Foldl, the blog", charset "utf-8"]
        body = hBody [
            header [
                nav [
                   a (HRef "/index.html") [em "Home"],
                   a (HRef "/articles.html") [em "Articles"],
                   a (HRef "/about.html") [em "About"],
                   a (HRef "/contact.html") [em "Contact"]
                ]
            ],
            content,
            footer [
                p [txt "Copyright Chris Coffey, 2018"]
                ]
            ]
