module About (
    aboutPage
) where

import Pachyderm.Web.HTML.Interface

aboutPage :: (Headings repr, Sectioning repr, Txt repr, TextElems repr, FlowContent repr, Lists repr) =>
    BodyBuilder (repr a)
aboutPage = article [
    h2 "About",
    p [txt "There are so many excellent tech blogs on the internet, so why am I writing yet another tech blog? Honestly, I'm doing this as a forcing function for my own thinking. The idea is that by publically presenting not only the finished product but also the process learning & creating it, I hope to clarify my own thinking."],
    p [txt "If other people find value in the process, that's even better, but its not my primary objective."],
    p [txt "As for me, I'm a programmer fascinated by typed functional programming. I also happen to be a father, rock climber, and engineering manager who works in startups."]
    ]






