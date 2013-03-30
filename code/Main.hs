{-# Language OverloadedStrings, TemplateHaskell #-}
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
import Text.Blaze.Internal (attribute)
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid
import Data.Aeson
import Data.Aeson.TH
import Data.Time

type Course        = Text
type Name          = Text
type StudentPoints = Int
type StudentID     = Text
type Major         = Text
type Degree        = Text
type Year          = Int
data Thesis        = Thesis Name [Course] deriving Show
data Date          = Date Year Season deriving (Show, Eq, Ord)
data Season        = Autumn | Spring deriving (Show, Eq, Ord)
data Student       = Student {
    date :: Date
  , studentId :: StudentID
  , name :: Name
  , studentPoints :: StudentPoints
  , degree ::  Degree
  , major :: Major
  } deriving Show

data StudentQueryRequest = StudentQueryRequest {
    studentRequestFirstName :: Maybe Name
  , studentRequestLastName :: Maybe Name
  , studentRequestPoints  :: Maybe (BinOp Int)
  , studentRequestDate  :: Maybe (BinOp Date)
  , studentRequestDegree  :: Maybe Degree
  , studentRequestMajor  :: Maybe Major
  } deriving Show
data BinOp a = AOR (BinOp a) (BinOp a) | AAND (BinOp a) (BinOp a) | AEQ a | AGT a | ALT a | AGTEQ a | ALTEQ a deriving Show

evalBinOp :: (Eq a, Ord a) => a -> BinOp a -> Bool
evalBinOp x (AEQ y) = x == y
evalBinOp x (ALT y) = x < y
evalBinOp x (AGT y) = x > y
evalBinOp x (AOR a b) = evalBinOp x a || evalBinOp x b
evalBinOp x (AAND a b) = evalBinOp x a && evalBinOp x b

newtype StudentQueryResponse = StudentQueryResponse ([Student])
$(deriveJSON id ''BinOp)
$(deriveJSON (drop 14) ''StudentQueryRequest)
$(deriveJSON id ''Season)
$(deriveJSON id ''Date)
$(deriveJSON id ''Student)
$(deriveJSON id ''StudentQueryResponse)

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
    H.title title
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport"    ! A.content "width=device-width, initial-scale=1.0"
    H.meta ! A.name "description" ! A.content ""
    H.meta ! A.name "author"      ! A.content ""
    H.link ! A.href "/static/bootstrap/css/bootstrap.css"            ! A.rel "stylesheet"
    H.link ! A.href "/static/css/style.css"                          ! A.rel "stylesheet"
    H.link ! A.href "/static/bootstrap/css/bootstrap-responsive.css" ! A.rel "stylesheet"
    H.script ! A.type_ "text/html" ! A.id "user-template" $
      H.div mempty
  H.body $ do
    H.div ! A.class_ "navbar navbar-inverse navbar-fixed-top" $ do
      H.div ! A.class_ "navbar-inner" $ do
        H.div ! A.class_ "container-fluid" $ do
          H.button ! A.type_ "button" ! A.class_ "btn btn-navbar" ! data_toggle "collapse" ! data_target ".nav-collapse" $ do
            H.span ! A.class_ "icon-bar" $ mempty
            H.span ! A.class_ "icon-bar" $ mempty
            H.span ! A.class_ "icon-bar" $ mempty
          H.a ! A.class_ "brand" ! A.href "#" $ title
          H.div ! A.class_ "nav-collapse collapse" $ do
            H.p ! A.class_ "navbar-text pull-right" $
              "Placeholder login text. What to do with this?"
            H.ul ! A.class_ "nav" $ do
              H.li $ "emptymenu"
    H.div ! A.class_ "container-fluid" $
      H.div ! A.class_ "row-fluid" $
        H.div ! A.class_ "span3" $
          H.div ! A.class_ "well sidebar-nav" $
            H.ul ! A.class_ "nav nav-list" $ do
              H.li ! A.class_ "nav-header" $ "Sidebar"
              H.li $ H.a ! A.href "#" $ "Link"
              H.li $ H.a ! A.href "#" $ "Link"
              H.li $ H.a ! A.href "#" $ "Link"
              H.li $ H.a ! A.href "#" $ "Link"
    H.div ! A.class_ "span9" $ do
      H.div ! A.class_ "hero-unit" $
        H.p "Main layout"
      H.div ! A.class_ "row-fluid" $
        H.div ! A.class_ "span4" $ do
          H.p "Smaller element"
      H.div ! A.class_ "row-fluid" $
        H.div ! A.class_ "span4" $ do
          H.p "Smaller element"
      H.div ! A.class_ "row-fluid" $
        H.div ! A.class_ "span4" $ do
          H.p "Smaller element"
  where
    data_toggle = attribute "data-toggle" "data-toggle=\""
    data_target = attribute "data-target" "data-target=\""
    title = "Käyttöliittymät harkka"

queryStudents :: [Student] -> ServerPart Response
queryStudents students = do
  query <- decode <$> lookBS "query"
  case query of
       Nothing -> ok $ toResponse $ encode $ StudentQueryResponse students
       Just query' -> ok $ toResponse $ encode $ StudentQueryResponse $ filter (buildFilter query') students
  where
    buildFilter query student = and . catMaybes $ [
        ((firstName' ==) <$> studentRequestFirstName query)
      , ((lastName' ==) <$> studentRequestLastName query)
      , ((degree student ==) <$> studentRequestDegree query)
      , ((major student ==) <$> studentRequestMajor query)
      , ((evalBinOp (studentPoints student)) <$> studentRequestPoints query)
      , ((evalBinOp (date student)) <$> studentRequestDate query)
      ]
      where
        (firstName', lastName') = T.breakOn " " (name student)

main :: IO ()
main = do
  thesis <- parseThesis "data/kandit.txt"
  students <- parseStudents "data/opiskelijat.txt"
  simpleHTTP nullConf $ msum [
      nullDir >> ok (toResponse mainView)
    , dir "students" $ (queryStudents students)
    , dir "static" $ serveDirectory EnableBrowsing [] "public/"
    ]
