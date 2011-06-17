-- | Hakyll project starter
--
module Main
    ( main
    ) where

import Control.Monad (forM_)
import System.Environment (getArgs, getProgName)
import System.FilePath (splitPath, joinPath, (</>))
import System.Directory (copyFile)

import Hakyll.Core.Util.File (getRecursiveContents, makeDirectories)

import Paths_hakyll_contrib (getDataDir)

-- | Available projects
--
projects :: [String]
projects =
    [ "small-blog"
    ]

-- | Start a project: just copy the directory
--
startProject :: String -> String -> IO ()
startProject p d = do
    source <- fmap (</> p) getDataDir
    contents <- getRecursiveContents False $ source
    forM_ contents $ \file -> do
        let dest = d </> stripDir source file
        putStrLn $ "Creating " ++ dest
        makeDirectories dest
        copyFile file dest
  where
    stripDir dir = joinPath . (drop $ length $ splitPath dir) . splitPath

-- | Print usage information
--
usage :: IO ()
usage = do
    progName <- getProgName
    putStr $ unlines
        [ "hakyll-contrib project starter"
        , ""
        , "Usage: " ++ progName ++ " <project> [dirname]"
        , ""
        , "Where project is one of:"
        ]
    forM_ projects $ \p -> putStrLn $ "    " ++ p

-- | Main function
--
main :: IO ()
main = do
    args <- getArgs
    case args of
        (p : d : _) -> if p `elem` projects then startProject p d else usage
        (p : _) -> if p `elem` projects then startProject p p else usage
        _ -> usage
