module Home (
    homePage
) where

import Pachyderm.Web.HTML.Interface

homePage :: (Headings repr, Sectioning repr, Txt repr, TextElems repr, FlowContent repr, Lists repr) =>
    BodyBuilder (repr a)
homePage = article [
    h2 "Foldl.io",
    p [txt "Welcome to Foldl.io. This blog was written from the ground up using Pachyderm, a modern, albeit 100% experimental static site framework written in Haskell. If you find any bugs or oddities, please let me know or open an issue on the pachyderm repository."],
    p [txt "If you're interested in hiring me, you can find a copy of my resume on this site. That being said, I'm extremely satisfied with my current employer and would be unlikely to leave."],
    p [txt "Enjoy!"]
    ]
