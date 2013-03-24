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
import Data.Maybe (maybe, catMaybes)
import Data.List (groupBy)
import Data.Function (on)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid

type Course        = Text
type Name          = Text
type StudentPoints = Int
type StudentID     = Text
type Major         = Text
type Degree        = Text
type Year          = Int
data Thesis        = Thesis Name [Course] deriving Show
data Date          = Date Year Season deriving Show
data Season        = Autumn | Spring deriving Show
data Student       = Student Date StudentID Name StudentPoints Degree Major deriving Show

parseStudents :: FilePath -> IO [Student]
parseStudents path = do
  catMaybes . map parseStudent . map (T.splitOn ";") .  T.splitOn "\r\n" <$> TI.readFile path
  where
    parseStudent (date : studentId : name : points : degree : major : _ ) = do
      let (date', season) = T.splitAt 4 date
      points' <- readMay (T.unpack points)
      date'' <- readMay $ T.unpack date'
      season' <- case T.unpack season of
                      "S" -> return Autumn
                      "K" -> return Spring
                      _ -> Nothing
      return $ Student (Date date'' season') studentId name points' degree major
    parseStudent _ = Nothing

parseThesis :: FilePath -> IO [Thesis]
parseThesis path = do
  contents <- TI.readFile path
  return $ maybe [] id $ parse contents
  where
    parseThesisCount (x:_) = readMay $ T.unpack x
    parseThesisNames xs n  = listToMaybe . take n . drop 1 $ xs
    listToMaybe []         = Nothing
    listToMaybe xs         = Just xs
    parse contents         = do
      let contents' = T.splitOn "\r\n" contents
      n <- parseThesisCount contents'
      names <- parseThesisNames contents' n
      courses <- listToMaybe $ groupBy ((==) `on` fst) [T.breakOn " " course | course <- drop (n+1) contents']
      return $ zipWith (\name course -> Thesis name (map snd course)) names courses

mainView :: Html
mainView = H.docTypeHtml $ do
  H.head $ do
    H.title "Käyttöliittymät harkka"
    H.script ! A.type_ "text/html" ! A.id "user-template" $
      H.div mempty
  H.body $ do
    mempty

main :: IO ()
main = simpleHTTP nullConf $ msum [
      nullDir >> ok (toResponse mainView)
    , dir "static" $ serveDirectory DisableBrowsing [] "public/"
  ]
