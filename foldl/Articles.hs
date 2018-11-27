module Articles (
    articles
) where

import Articles.Sample
import Pachyderm.Web.HTML.Interface
import Control.Monad.Reader
import Data.ByteString

articles :: (FlowContent repr, Lists repr) =>
    BodyBuilder (repr a)
articles = ul [
        toArticleLink undefined,
        toArticleLink undefined,
        toArticleLink undefined,
        toArticleLink undefined
    ]
    where
        toArticleLink = undefined


-- articleToElement

data BlogArticle xs repr a =
    BlogArticle {
        linkTitle :: ByteString,
        publishDate :: ByteString,
        content :: [Reader xs (repr a)]
    }
