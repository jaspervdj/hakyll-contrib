-- | This module provides a simple default configuration which behaves similar
-- to the Jekyll static site generator.
--
-- <http://jekyllrb.com/>
--
-- The idea is that you don't have to write your configuration yourself: you
-- just follow some conventions, and Hakyll does the rest.
--
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Hakyll.Contrib.SmallBlog
    ( SmallBlogConfiguration
    , defaultSmallBlogConfiguration
    , smallBlog
    , smallBlogWith
    ) where

import Control.Arrow ((>>>))

import Text.Pandoc (ParserState, WriterOptions)

import Hakyll

-- | Configuration datatype for the 'smallBlog' ruleset
--
data SmallBlogConfiguration = SmallBlogConfiguration
    { -- | Number of recent posts that are available
      numberOfRecentPosts :: Int
    , -- | Parser state for pandoc, i.e. read options
      parserState         :: ParserState
    , -- | Writer options for pandoc
      writerOptions       :: WriterOptions
    } deriving (Show)

-- | Defaults for 'SmallBlogConfiguration'
--
defaultSmallBlogConfiguration :: SmallBlogConfiguration
defaultSmallBlogConfiguration = SmallBlogConfiguration
    { numberOfRecentPosts = 3
    , parserState         = defaultHakyllParserState
    , writerOptions       = defaultHakyllWriterOptions
    }

-- | A default configuration for a small blog
--
smallBlog :: Rules
smallBlog = smallBlogWith defaultSmallBlogConfiguration

-- | Version of 'smallBlog' which allows setting a config
--
smallBlogWith :: SmallBlogConfiguration -> Rules
smallBlogWith conf = do
    -- Images and static files
    ["favicon.ico"]           --> copy
    ["img/**", "images/**"]   --> copy
    ["static/**", "files/**"] --> copy
    ["js/**", "javascript/**"] --> copy

    -- CSS files
    ["css/*.css", "style/*.css", "stylesheets/*.css"] --> css

    -- All templates
    ["templates/*"] --> template

    -- "Dynamic" content
    ["posts/*"] --> post

    -- Top-level pages
    ["*.markdown", "*.html", "*.rst", "*.lhs"] --> topLevel
  where
    -- Useful combinator here
    xs --> f = mapM_ (\p -> match p $ f) xs

    -- Completely static
    copy = route idRoute >> compile copyFileCompiler
    
    -- CSS directories
    css = route (setExtension "css") >> compile compressCssCompiler

    -- Templates
    template = compile templateCompiler

    -- Posts
    post = do
        route $ setExtension "html"
        compile $ pageCompilerWith (parserState conf) (writerOptions conf)
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Top-level pages
    topLevel = do
        route $ setExtension "html"
        compile $ pageCompilerWithFields (parserState conf)
            (writerOptions conf) id topLevelFields
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    -- Add the fields we need to top-level pages
    topLevelFields = setFieldPostList recentFirst "allPosts"
        >>> setFieldPostList (take nRecent . recentFirst) "recentPosts"
        >>> setFieldPostList chronological "chronologicalPosts"

    -- Create a post list based on ordering/selection
    setFieldPostList f k = setFieldPageList f
        "templates/post-item.html" k "posts/*"

    -- Number of most recent posts to show
    nRecent = numberOfRecentPosts conf
