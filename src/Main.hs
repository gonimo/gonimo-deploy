{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import qualified Data.ByteString.Char8               as B'
import qualified Data.ByteString.Lazy.Char8          as B
import           System.Console.GetOpt
{-import           Data.ByteString.Lazy.Char8  (ByteString)-}
import qualified Data.ByteString.Lazy.Search         as B
import           Data.Conduit                        ((=$=), (.|))
import qualified Data.Conduit                        as C
import qualified Data.Conduit.Binary                 as C
import qualified Data.Conduit.List                   as C
import           Data.Digest.Pure.MD5                as MD5
import qualified Data.Map                            as M
import           Data.Monoid                         ((<>))
import           System.Directory                    (createDirectoryIfMissing)
import           System.FilePath
import           Data.List                           ((\\))
import           Data.Conduit.Combinators (sinkList, sourceDirectoryDeep)
import           System.Environment (getArgs, getProgName)

blacklist :: [String]
-- | list of extensions
blacklist = [ ".jpg", ".jpeg", ".png", ".gif"
            , ".mp3", ".wav", ".ogg"
            , ".otf", ".woff", ".woff2", ".ttf", ".eot"
            ]

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    ["md5sum", path] -> md5sumAll path
    _ -> putStrLn $ "Usage: " <> progName <> " md5sum " <> "distRoot"


md5sumAll :: FilePath -> IO ()
md5sumAll root = do
  allFiles <- C.runConduitRes $ sourceDirectoryDeep True root .| sinkList
  allContents <- traverse B.readFile allFiles
  let makeMapEntry path content =
        let
          packedPath = B'.pack . removeRoot root $ path
          hashedPath = packedPath <> "?" <> B'.pack (show (md5 content))
        in
          (packedPath, hashedPath)

  let md5Sums = M.fromList $ zipWith makeMapEntry allFiles allContents
  let md5Files = filter ((`notElem` blacklist) . takeExtensions) allFiles
  let replaceContents content = M.foldrWithKey B.replace (B.fromStrict content) md5Sums

  forM_ md5Files $ \fileName ->
    C.runConduitRes
    $  C.sourceFile fileName
    =$= C.map (B.toStrict . replaceContents)
    =$= C.sinkFileCautious fileName

removeRoot :: FilePath -> FilePath -> FilePath
removeRoot root path = path \\ addTrailingPathSeparator root -- Trailing path seperator needed because of weird </> behaviour!
