module Main where

import Layout
import Articles
import Home
import About

import Control.Monad (when)
import Pachyderm.Web.HTML.Render (render)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    createDirectoryIfMissing True "articles"
    mapM_ emitContent $ corePages<>contentPages

emitContent :: RenderedPage -> IO ()
emitContent (CP n c) =
    BS.writeFile (BSC.unpack n) c

corePages = [
    home
    , about
--    contact,
    , articlesIndex
    ]


data RenderedPage = CP {
    cpLocation :: BS.ByteString,
    cpContent :: BS.ByteString
    }

contentPages :: [RenderedPage]
contentPages = toContent <$> articles
    where
        toContent BlogArticle {name, content} =
            CP {
                cpLocation = "articles/"<>name,
                cpContent = render $ blogLayout content
            }
home :: RenderedPage
home = CP "index.html" $ render (blogLayout homePage)
about :: RenderedPage
about = CP "about.html" $ render (blogLayout aboutPage)
articlesIndex :: RenderedPage
articlesIndex = CP "articles.html" . render . blogLayout $ articleLinks articles
