module Web.HTML.Basic  where

import Pachyderm.HList (Empty, (:.)(..), Elem(..))
import Pachyderm.Web.HTML.Interface (HtmlDoc(..), HeadNode(..), Headings(..),
    TextElems(..), Txt(..), FlowContent(..), Sectioning(..), TextSegment, Head, Body, GenBuilder,
    HeadNodeBuilder, BodyBuilder, HRef(..))


dummyDoc' :: (HtmlDoc repr, HeadNode repr, Headings repr,
    TextElems repr, FlowContent repr, Txt repr, Sectioning repr) =>
    repr a
dummyDoc' =
    hDoc heading body
    where
        heading = hHead [title "Test Page II", charset "utf-8"]
        body = hBody [
            header [
                nav [
                   a (Href "#") [em "Home"],
                   a (Href "#") [em "Articles"],
                   a (Href "#") [em "About"],
                   a (Href "#") [em "Contact"]
                ]
            ],
            article [
                p [
                    txt "Test Doc",
                    code "things and such -> great success"
                    ],
                h3 "Moar content",
                p [
                    txt "writing stuff"
                    ]
                ],
            footer [
                p [txt "By me"]
                ]
            ]
