module Articles (
    articleLinks,
    articles,
    BlogArticle(..)
) where

import Prelude hiding (div)
import qualified Articles.Sample as Sample
import Pachyderm.Web.HTML.Interface
import Pachyderm.HList
import Control.Monad.Reader
import Data.ByteString

articleLinks :: (FlowContent repr, Lists repr, Txt repr, TextElems repr) =>
    [BlogArticle (repr a)]
    -> BodyBuilder (repr a)
articleLinks =  div . fmap toArticleLink
    where
        toArticleLink article = p [
                a (HRef $ "articles/" <> name article) [txt $ linkTitle article],
                i $ "published on: "<> publishDate article
            ]

articles :: (Headings repr, Sectioning repr, Txt repr,
    TextElems repr, FlowContent repr, Lists repr) =>
    [BlogArticle (repr a)]
articles = [
    BlogArticle {
        name = "pachyderm_intro.html",
        linkTitle = "Introducing Pachyderm",
        publishDate = "11/30/2018",
        content = Sample.samplePost
        }
    ]

data BlogArticle repr =
    BlogArticle {
        name :: ByteString, -- TODO newtype this to make it URL encoded
        linkTitle :: ByteString,
        publishDate :: ByteString,
        content :: BodyBuilder repr
    }
