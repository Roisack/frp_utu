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
import Data.Maybe (maybe, catMaybes, listToMaybe)
import Data.List (groupBy, sortBy, group)
import Data.Function (on)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Internal (attribute)
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid
import Data.Aeson
import Data.Aeson.TH
import Data.Time
import qualified Data.Set as S
import Data.Set (Set)
import Data.ByteString.Lazy (ByteString)

type Course        = Text
type Name          = Text
type StudentPoints = Int
type StudentID     = Text
type Major         = Text
type Degree        = Text
type Year          = Int
data Thesis        = Thesis {
    thesisName :: Name
  , thesisCourses :: Set Course
  } deriving (Show, Eq, Ord)
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

data ThesisQueryRequest = StudentThesisRequest {
    thesisQueryName :: Maybe Name
  , thesisQueryCourses :: [Course]
  }
data StudentQueryRequest = StudentQueryRequest {
    studentQueryFirstName :: Maybe Name
  , studentQueryLastName :: Maybe Name
  , studentQueryPoints  :: Maybe (BinOp Int)
  , studentQueryDate  :: Maybe (BinOp Date)
  , studentQueryDegree  :: Maybe Degree
  , studentQueryMajor  :: Maybe Major
  } deriving Show
data StudentSortRequest = StudentSortByDate SortOrder | StudentSortById SortOrder | StudentSortByName SortOrder | StudentSortByPoints SortOrder | StudentSortByDegree SortOrder | StudentSortByMajor SortOrder
data ThesisSortRequest = ThesisSortByName SortOrder | ThesisSortByCourses SortOrder
data SortOrder = Asc | Desc deriving Show
data BinOp a = AOR (BinOp a) (BinOp a) | AAND (BinOp a) (BinOp a) | AEQ a | AGT a | ALT a | AGTEQ a | ALTEQ a deriving Show

evalBinOp :: (Eq a, Ord a) => a -> BinOp a -> Bool
evalBinOp x (AEQ y) = x == y
evalBinOp x (ALT y) = x < y
evalBinOp x (AGT y) = x > y
evalBinOp x (AOR a b) = evalBinOp x a || evalBinOp x b
evalBinOp x (AAND a b) = evalBinOp x a && evalBinOp x b

newtype StudentQueryResponse = StudentQueryResponse ([Student])
newtype ThesisQueryResponse = ThesisQueryResponse ([Thesis])
$(deriveJSON id ''BinOp)
$(deriveJSON (drop 12) ''StudentQueryRequest)
$(deriveJSON id ''Season)
$(deriveJSON id ''Date)
$(deriveJSON id ''Student)
$(deriveJSON id ''StudentQueryResponse)
$(deriveJSON id ''ThesisQueryResponse)
$(deriveJSON id ''SortOrder)
$(deriveJSON id ''StudentSortRequest)
$(deriveJSON id ''ThesisSortRequest)
$(deriveJSON (drop 6) ''Thesis)
$(deriveJSON (drop 11) ''ThesisQueryRequest)

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
      return $ zipWith (\name course -> Thesis name (S.fromList $ map snd course)) names courses

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
  H.script ! A.type_ "application/javascript" ! A.src "/static/jquery/jquery-1.9.1.min.js" $ mempty
  H.script ! A.type_ "application/javascript" ! A.src "/static/bacon/js/Bacon.js" $ mempty
  H.script ! A.type_ "application/javascript" ! A.src "/static/mustache/mustache.js" $ mempty
  H.script ! A.type_ "application/javascript" ! A.src "http://datatables.net/download/build/jquery.dataTables.nightly.js" $ mempty
  H.script ! A.type_ "application/javascript" ! A.src "/static/js/doThings.js" $ mempty
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
      H.div ! A.class_ "row-fluid" $ do
        H.div ! A.id "table_mode" $ do
          H.button ! A.id "mode_students" $ "Students"
          H.button ! A.id "mode_degrees" $ "Degrees"
          H.button ! A.id "mode_courses" $ "Courses"
        H.div ! A.id "controlpanel" $ do
          H.h2 $ "Filters"
          H.form ! A.id "coolform" $ do
            H.input ! A.type_ "text" ! A.placeholder "Enter filter text" ! A.id "filterstring"
            H.p $ "Apply filter for"
            H.label ! A.type_ "text" $ "First name"
            H.input ! A.type_ "checkbox" ! A.id "firstName"
            H.label ! A.type_ "text" $ "Last name"
            H.input ! A.type_ "checkbox" ! A.id "lastName"
            H.label ! A.type_ "text" $ "Student number"
            H.input ! A.type_ "checkbox" ! A.id "studentNumber"
            H.label ! A.type_ "text" $ "Degree"
            H.input ! A.type_ "checkbox" ! A.id "degree"
            H.label ! A.type_ "text" $ "Points"
            H.input ! A.type_ "checkbox" ! A.id "points"
            H.label ! A.type_ "text" $ "Major"
            H.input ! A.type_ "checkbox" ! A.id "major"
            H.br
            H.br
            H.button ! A.id "form_submit" ! A.class_ "btn" $ "Execute"
      H.div ! A.class_ "hero-unit" ! A.id "databox" $ do
        H.p $ "stuff here"
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

lookBSsafe :: (Monad m, Functor m, HasRqData m) => String -> m (Maybe ByteString)
lookBSsafe key = listToMaybe <$> lookBSs key

queryStudents :: [Student] -> ServerPart Response
queryStudents students = do
  query <- join . fmap decode <$> lookBSsafe "query"
  ok .
    toResponse .
    encode .
    StudentQueryResponse .
    maybe students (\q -> filter (buildFilter q) students) $ query
  where
    sort (s:ss) students = concat $ map (sort ss) $ groupBy (groupFun s) $ sortBy (sortFun s) students
    sortDir Asc = id
    sortDir Desc = flip
    sortFun (StudentSortByDate dir)   = sortDir dir (compare `on` date)
    sortFun (StudentSortById dir)     = sortDir dir (compare `on` studentId)
    sortFun (StudentSortByName dir)   = sortDir dir (compare `on` name)
    sortFun (StudentSortByPoints dir) = sortDir dir (compare `on` studentPoints)
    sortFun (StudentSortByDegree dir) = sortDir dir (compare `on` degree)
    sortFun (StudentSortByMajor dir)  = sortDir dir (compare `on` major)
    groupFun (StudentSortByDate _)   = (==) `on` date
    groupFun (StudentSortById _)     = (==) `on` studentId
    groupFun (StudentSortByName _)   = (==) `on` name
    groupFun (StudentSortByPoints _) = (==) `on` studentPoints
    groupFun (StudentSortByDegree _) = (==) `on` degree
    groupFun (StudentSortByMajor _)  = (==) `on` major
    buildFilter query student = and . catMaybes $ [
        ((firstName' ==) <$> studentQueryFirstName query)
      , ((lastName' ==) <$> studentQueryLastName query)
      , ((degree student ==) <$> studentQueryDegree query)
      , ((major student ==) <$> studentQueryMajor query)
      , ((evalBinOp (studentPoints student)) <$> studentQueryPoints query)
      , ((evalBinOp (date student)) <$> studentQueryDate query)
      ]
      where
        (firstName', lastName') = T.breakOn " " (name student)

queryThesis :: [Thesis] -> ServerPart Response
queryThesis thesis = do
  query <- join . fmap decode <$> lookBSsafe "query"
  ok .
    toResponse .
    encode .
    ThesisQueryResponse .
    maybe thesis (\q -> filter (buildFilter q) thesis) $ query
  where
    sort (s:ss) thesis = concat $ map (sort ss) $ groupBy (groupFun s) $ sortBy (sortFun s) thesis
    sortDir Asc = id
    sortDir Desc = flip
    sortFun (ThesisSortByName dir) = sortDir dir (compare `on` thesisName)
    sortFun (ThesisSortByCourses dir) = sortDir dir (compare `on` thesisCourses)
    groupFun (ThesisSortByCourses _) = (==) `on` thesisCourses
    groupFun (ThesisSortByName _) = (==) `on` thesisName
    buildFilter query thesis = and . catMaybes $ [
        (thesisName thesis ==) <$> thesisQueryName query
      , Just $ (S.fromList $ thesisQueryCourses query) `S.isSubsetOf` (thesisCourses thesis)
      ]

main :: IO ()
main = do
  thesis <- parseThesis "data/kandit.txt"
  students <- parseStudents "data/opiskelijat.txt"
  simpleHTTP nullConf{port=25565} $ do
    decodeBody (defaultBodyPolicy "/tmp" 4096 4096 4096)
    msum [
        nullDir >> ok (toResponse mainView)
      , dir "students" $ (queryStudents students)
      , dir "thesis" $ (queryThesis thesis)
      , dir "static" $ serveDirectory EnableBrowsing [] "public/"
      ]

