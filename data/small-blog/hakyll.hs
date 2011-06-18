{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll
import Hakyll.Contrib.SmallBlog

main :: IO ()
main = hakyll $ smallBlogWith defaultSmallBlogConfiguration
    { atomFeed = Just FeedConfiguration
        { feedTitle       = "A simple blog"
        , feedDescription = "Certainly very enjoyable yes"
        , feedAuthorName  = "Jasper Van der Jeugt"
        , feedRoot        = "http://example.com"
        }
    }
