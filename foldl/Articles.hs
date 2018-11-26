module Articles (
    articles
) where

import Articles.Sample

articles :: (FlowContent repr, Lists repr) =>
    BodyBuilder (repr a)
articles = undefined

data BlogArticle xs repr a =
    BlogArticle {
        linkTitle :: ByteString,
        publishDate :: ByteString,
        content :: [Reader xs (repr a)]
    }
