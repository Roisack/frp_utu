{-# Language OverloadedStrings #-}
module Main where

import Safe
import Happstack.Server
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Data.Text.Lazy (Text)
import qualified Data.Vector as V
import qualified System.IO as IO
import Control.Applicative
import Control.Monad
import Data.Maybe (maybe)
import Data.List (groupBy)
import Data.Function (on)

type Course = Text
data Thesis = Thesis Text [Course] deriving Show

parseThesis :: FilePath -> IO [Thesis]
parseThesis path = do
  contents <- TI.readFile path
  return $ maybe [] id $ parse contents
  where
    parseThesisCount (x:_) = readMay $ T.unpack x
    parseThesisNames xs n = listToMaybe . take n . drop 1 $ xs
    listToMaybe [] = Nothing
    listToMaybe xs = Just xs
    parse :: Text -> Maybe [Thesis]
    parse contents = do
      let contents' = T.lines contents
      n <- parseThesisCount contents'
      names <- parseThesisNames contents' n
      courses <- listToMaybe $ groupBy ((==) `on` fst) [T.breakOn " " course | course <- drop (n+1) contents']
      return $ zipWith (\name course -> Thesis name (map snd course)) names courses

main = simpleHTTP nullConf $ ok ("Hello, world" :: Text)
