-- | This module provides a simple default configuration which behaves similar
-- to a tool such as the Jekyll static site generator (<http://jekyllrb.com/>).
--
-- The idea is that you don't have to write your configuration yourself: you
-- just follow some conventions, and Hakyll does the rest.
--
-- You can generate a site which will serve as a good starting point by running
-- the command-line tool:
--
-- > hakyll-contrib small-blog
--
-- Hakyll will then generate a simple example site for you. The necessary
-- configuration is placed in the @hakyll.hs@ file. Compile and run it to create
-- the demo site:
--
-- > ghc --make hakyll.hs
-- > ./hakyll build
-- > ./hakyll preview
--
-- So, in order to get your site going, you need to follow the conventions for
-- the content on your site.
--
-- Images should be placed in the @images\/@ or @img\/@ folder. The are copied
-- directly. Other static files (but not images) can be placed in @static\/@ or
-- @files\/@. The @favicon.ico@ file is an exception, it is just placed in the
-- top-level directory.
--
-- CSS files should be placed in @css\/@, and JavaScript files in @js\/@.
--
-- Then, we arrive at pages. You can create any number of pages on your site:
-- just create files in one of the documents pandoc supports (@.html@,
-- @.markdown@, @.rst@, @.lhs@...) in the top-level directory.
--
-- These pages may use a number of preconfigured @$key$@'s:
--
-- * @$recentPosts$@: A list of recent posts, displayed from most recent to
--   oldest. By default, 3 posts are shown, altough this can be configured using
--   the 'numberOfRecentPosts' field.
--
-- * @$allPosts$@: A list of all posts, displayed from most recent to oldest.
--   This is very useful for creating an archive page.
--
-- * @$chronologicalPosts$@: Similar to @$allPosts$@, but displays the posts in
--   chronological order.
--
-- For example usage, look at the example site we generated using
-- @hakyll-contrib small-blog@.
--
-- Now, one can wonder where these posts come from. Simple: all pages in the
-- @posts\/@ directory are considered posts. Note that a naming format of
--
-- > posts/year-month-date-title.extension
--
-- is mandatory. An example:
--
-- > posts/2011-06-19-hello-world.markdown
--
-- This allows Hakyll to parse the date easily, among other things. Again, look
-- at the example site for some example posts.
--
-- Additionaly, there is the @templates\/@ folder. This folder holds the
-- templates for your site. For a @small-blog@ configuration, your site should
-- have /exactly/ three templates:
--
-- * @templates\/default.html@: The main template. This should contain your
--   HTML doctype, head, etc.
--
-- * @templates\/post.html@: A template which is applied to every post before
--   it is rendered using the default template.
--
-- * @templates\/post-item.html@: A template which is applied to posts in
--   listings (e.g. @$chronologicalPosts$@).
--
-- Again, the example should clarify things.
--
-- This configuration should be enough to create a small personal website. But,
-- we have only touched the surface of what is possible with Hakyll. For more
-- information, check out the tutorials at: <http://jaspervdj.be/hakyll>
--
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Hakyll.Contrib.SmallBlog
    ( SmallBlogConfiguration (..)
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
    , -- | Atom feed configuration
      atomFeed            :: Maybe FeedConfiguration
    } deriving (Show)

-- | Defaults for 'SmallBlogConfiguration'
--
defaultSmallBlogConfiguration :: SmallBlogConfiguration
defaultSmallBlogConfiguration = SmallBlogConfiguration
    { numberOfRecentPosts = 3
    , parserState         = defaultHakyllParserState
    , writerOptions       = defaultHakyllWriterOptions
    , atomFeed            = Nothing
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

    -- Rss is optional
    case atomFeed conf of
        Nothing -> return ()
        Just f  -> do
            match  "atom.xml" $ route idRoute
            create "atom.xml" $ requireAll_ "posts/*" >>> renderAtom f
            return ()
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
