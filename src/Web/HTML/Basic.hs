module Web.HTML.Basic  where

import Pachyderm.HList (Empty, (:.)(..), Elem(..))
import Pachyderm.Web.HTML.Interface (HtmlDoc(..), HeadNode(..), Headings(..),
    TextElems(..), Txt(..), FlowContent(..), TextSegment, Head, Body, GenBuilder,
    HeadNodeBuilder, BodyBuilder)


dummyDoc' :: (HtmlDoc repr, HeadNode repr, Headings repr,
    TextElems repr, FlowContent repr, Txt repr) =>
    repr a
dummyDoc' =
    hDoc heading body
    where
        heading = hHead [title "Test Page II", charset "utf-8"]
        body = hBody [
            p [
                txt "Test Doc",
                code "things and such -> great success"
                ],
            h3 "Moar content",
            p [
                txt "writing stuff"
                ]
            ]
