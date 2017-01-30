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
import qualified Data.ByteString.Search         as B'
import           Data.Conduit                        ((=$=), (.|))
import qualified Data.Conduit                        as C
import qualified Data.Conduit.Binary                 as C
import qualified Data.Conduit.List                   as C
import           Data.Digest.Pure.MD5                as MD5
import qualified Data.Map                            as M
import           Data.Monoid                         ((<>))
import           System.FilePath
import           Data.List                           ((\\))
import           Data.Conduit.Combinators (sinkList, sourceDirectoryDeep)
import           System.Environment (getArgs, getProgName)
import           Data.Bifunctor (bimap)
import           System.Directory (renameFile, setCurrentDirectory)
import           Data.Foldable (traverse_)

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
          packedExtension = B.pack . takeExtensions $ path
          packedPathNoExt = B.pack . removeRoot root . dropExtensions $ path
          packedPath = packedPathNoExt <> packedExtension
          hashedPath = packedPathNoExt <> "-" <> B.pack (show (md5 content)) <> packedExtension
        in
          (packedPath, hashedPath)

  let md5SumList = zipWith makeMapEntry allFiles allContents
  let md5Sums = M.fromList md5SumList
  let doEscapes = B.replace "/" ("\\/" :: B.ByteString)
  let md5SumsEscaped' = M.fromList $ bimap doEscapes doEscapes <$> md5SumList -- ghcjs escapes /!
  let md5SumsEscaped = md5SumsEscaped' `M.difference` md5Sums -- Paths without a / are equal!
  let md5Files = filter ((`notElem` blacklist) . takeExtensions) allFiles
  writeHashes md5Files md5Sums
  writeHashes md5Files md5SumsEscaped
  -- Rename instead of ?hash trick, because this also works for php includes:
  setCurrentDirectory root
  traverse_ (uncurry renameFile . bimap B.unpack B.unpack) md5SumList

writeHashes :: [FilePath] -> M.Map B.ByteString B.ByteString -> IO ()
writeHashes md5Files md5Sums = do
  let
    saneReplace :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
    saneReplace needle rep haystack = B.replace (B.toStrict needle) rep haystack

    replaceContents :: B.ByteString -> B.ByteString
    replaceContents content = M.foldrWithKey saneReplace content md5Sums

    replaceContentsRelative :: FilePath -> B.ByteString -> B.ByteString
    replaceContentsRelative fileName content =
      let
        fileDir = takeDirectory fileName
        makeLocal = B.pack . removeRoot fileDir . B.unpack
        fileMapLocal = M.fromList . map (bimap makeLocal makeLocal) . M.toList $ md5Sums
      in
        M.foldrWithKey saneReplace content fileMapLocal

  forM_ md5Files $ \fileName ->
    C.runConduitRes
    $  C.sourceFile fileName
    =$= C.map (B.toStrict . replaceContents . B.fromStrict)
    =$= C.map (B.toStrict . replaceContentsRelative fileName . B.fromStrict) -- Limited support for relative links needed for bootstrap css files.
    =$= C.sinkFileCautious fileName

removeRoot :: FilePath -> FilePath -> FilePath
removeRoot root path = path \\ addTrailingPathSeparator root -- Trailing path seperator needed because of weird </> behaviour!
