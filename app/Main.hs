{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import System.Directory
import Data.List (isSuffixOf)
import Data.Yaml
import Data.Tuple (swap)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Lens ( (^?) )
import Data.Aeson.Lens ( key )
import Data.String (fromString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

main :: IO ()
main = do
    codeMetaFileMap <- getCodeMetaGuidFileMap
    prefabs <- getPrefabs

    print prefabs

filenameAndYamlMultiple :: FilePath -> IO (FilePath, [Value])
filenameAndYamlMultiple f = do
    yaml <- fixUnityYaml <$> S.readFile f
    pure (f, exceptionIntoString $ decodeAllEither' yaml)

filenameAndYaml :: FilePath -> IO (FilePath, Value)
filenameAndYaml = (fmap.fmap.fmap) (!!1) filenameAndYamlMultiple

getCodeMetaGuidFileMap :: IO (M.Map Value FilePath)
getCodeMetaGuidFileMap = withCurrentDirectory "Assembly-CSharp" do
    codeMetaFilepaths <- filter (isSuffixOf ".cs.meta") <$> listDirectory "."
    metaGuidToFilenameMap <$> traverse filenameAndYaml codeMetaFilepaths

getPrefabs :: IO [(FilePath, [Value])]
getPrefabs = withCurrentDirectory "GameObject" do
    prefabFilepaths <- filter (isSuffixOf ".prefab") <$> listDirectory "."
    traverse filenameAndYamlMultiple prefabFilepaths




metaGuidToFilenameMap :: [(FilePath, Value)] -> M.Map Value FilePath
metaGuidToFilenameMap fileGuidPairs = M.fromList $ fmap swap $ fmap (fromMaybe "" . (^? key "guid")) <$> fileGuidPairs

fixUnityYaml :: S.ByteString -> S.ByteString
fixUnityYaml y = S8.unlines $ duplicateTag "" $ S8.lines y

duplicateTag :: S.ByteString -> [S.ByteString] -> [S.ByteString]
duplicateTag t (a:as)
 | S8.isPrefixOf "%TAG" a = duplicateTag a as
 | S8.isPrefixOf "--- !u!" a = t : a : duplicateTag t as
 | otherwise = a : duplicateTag t as
duplicateTag _ [] = []

exceptionIntoString :: Either ParseException [Value] -> [Value]
exceptionIntoString (Left a) = [String $ fromString $ show a]
exceptionIntoString (Right a) = a
