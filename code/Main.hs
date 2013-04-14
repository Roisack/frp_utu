{-# Language OverloadedStrings, TemplateHaskell #-}
module Main where

import Safe
import Happstack.Server
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import qualified Data.Text.Lazy.Encoding as E
import Data.Text.Lazy (Text)
import qualified Data.Vector as V
import qualified System.IO as IO
import Control.Applicative
import Control.Monad
import Data.Maybe (maybe, catMaybes, listToMaybe)
import Data.List (groupBy, sortBy, group, find)
import Data.Function (on)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (AttributeValue, Html, (!))
import Text.Blaze.Internal (attribute)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid
import Data.Aeson
import Data.Aeson.TH
import Data.Time
import qualified Data.Set as S
import Data.Set (Set, (\\))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Monad.State

data MinedData = MinedData {
    students :: Map StudentID Student
  , thesis :: [Thesis]
  , credits :: [Credit]
  }
type Mining a = StateT MinedData (ServerPartT IO) a

type CourseCode    = Text
type Name          = Text
type StudentPoints = Int
type StudentID     = Text
type Major         = Text
type Degree        = Text
type Year          = Int
data Course = Course {
    courseCode :: CourseCode
  , courseCredits :: Int
  }
data Thesis        = Thesis {
    thesisName :: Name
  , thesisCourses :: Set CourseCode
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
data Credit = Credit {
    creditStudentId :: StudentID
  , creditId :: Text
  , creditName :: Text
  , creditDate :: Text
  , creditCredits :: Int
  } deriving Show
newtype DatatableCourse = DatatableCourse Credit
newtype DatatableStudent = DatatableStudent Student
newtype DatatableThesis = DatatableThesis Thesis

$(deriveJSON id ''Season)
$(deriveJSON id ''Date)
$(deriveJSON id ''Student)
$(deriveJSON id ''Credit)
$(deriveJSON id ''Thesis)

instance ToJSON DatatableCourse where
  toJSON (DatatableCourse credit) = toJSON [
        creditId credit
      , creditName credit
      , T.pack . show . creditCredits $ credit
    ]

instance ToJSON DatatableStudent where
  toJSON (DatatableStudent student) = toJSON [
        studentId student
      , name student
      , degree student
      , major student
      , T.pack $ show $ studentPoints student
      , T.pack $ drop 5 $ show $ date student
    ]

instance ToJSON DatatableThesis where
  toJSON (DatatableThesis thesis) = toJSON [
        thesisName thesis
      , T.pack $ show $ S.size $ thesisCourses thesis
    ]

instance ToMessage Value where
  toMessage x = encode x
  toContentType _ = B.pack ("application/json" :: String)

parseCredits :: FilePath -> IO [Credit]
parseCredits path =
  catMaybes . map parseCredit . map (T.splitOn ";") .  T.splitOn "\r\n" <$> TI.readFile path
  where
    parseCredit (sid : cid : name : date : credits : _ ) = do
      credits' <- readMay $ T.unpack credits
      return $ Credit sid cid name date credits'
    parseCredit _ = Nothing

parseStudents :: FilePath -> IO (Map StudentID Student)
parseStudents path =
  toMap . catMaybes . map parseStudent . map (T.splitOn ";") .  T.splitOn "\r\n" <$> TI.readFile path
  where
    toMap students = M.fromList [(studentId s, s) | s <- students]
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

studentModal :: Html
studentModal = H.div ! A.id "modal" ! A.class_ "modal hide fade" $ do
  H.div ! A.class_ "modal-header" $ do
    H.button ! A.type_ "button" ! A.class_ "close" ! data_dismiss "modal" ! aria_hidden "true" $
      "x"
    H.h3 ! A.id "student_header" $ mempty
  H.div ! A.class_ "modal-body" ! A.id "modalBody" $ do
    mempty
  H.div ! A.class_ "modal-footer" $ do
    H.a ! A.href "#" ! A.class_ "btn close" $ "Close"
  where
    data_dismiss = attribute "data-dismiss" "data-dismiss=\""
    aria_hidden = attribute "aria-hidden" "aria-hidden=\""

studentQuery :: Mining Response
studentQuery = do
  students <- gets students
  thesis <- gets thesis
  credits <- gets credits
  id' <- lookText "studentId"
  let student = M.lookup id' students
  case student of
       Just student' -> ok $ toResponse $ toJSON $ studentObject thesis credits student'
       Nothing -> notFound $ toResponse $ notFoundView $
         H.p "User is not found"
  where
    studentObject thesis credits student = let
      studentsCredits = sum $ map creditCredits studentsCourses
      studentsCourses = filter (\c -> creditStudentId c == studentId student) credits
      requiredCourses = maybe S.empty thesisCourses $ find (\t -> thesisName t == degree student) thesis
      missingCourses = [object ["thesis" .= thesisName t, "missingCourses" .= S.toList (thesisCourses t \\ S.fromList (map creditName studentsCourses))] | t <- thesis]
      in object [
              "student" .= student
            , "studentsCredits" .= (T.pack . show $ studentsCredits)
            , "studentsCourses" .= studentsCourses
            , "missingCourses" .= missingCourses
          ]

fileResponse ::  Html -> Html
fileResponse fun = H.docTypeHtml $ do
  H.head $
    H.script ! A.type_ "application/javascript" $
      ("parent." `mappend` fun `mappend` "()")
  H.body $ mempty

unionCoursesData :: Mining Response
unionCoursesData = do
  t <- gets thesis
  ok . toResponse . toJSON . unionCourses $ t
  where
    unionCourses thesis = S.toList $ foldr (\t s -> S.union s (thesisCourses t)) mempty thesis

creditsData :: Mining Response
creditsData = do
  c <- gets credits
  ok . toResponse . toJSON . combineResults $ c
  where
    combineResults credits = let
      courses = foldr (\c m -> M.insert (creditId c) (creditId c, creditName c, creditCredits c) m) mempty credits
      in [[id', name, T.pack . show $ credit] | (id', name, credit) <- M.elems courses]

thesisData :: Mining Response
thesisData = do
  t <- gets thesis
  ok $ toResponse $ toJSON $ map DatatableThesis $ t

studentsData :: Mining Response
studentsData = do
  s <- gets students
  ok $ toResponse $ toJSON $ map DatatableStudent $ M.elems s

studentsUpload :: Mining Response
studentsUpload = do
  (path, _, _) <- lookFile "studentFile"
  newStudents <- liftIO $ parseStudents path
  modify (\m -> m{students=newStudents})
  ok $ toResponse $ fileResponse "touchStudents"

thesisUpload :: Mining Response
thesisUpload = do
  (path, _, _) <- lookFile "thesisFile"
  newThesis <- liftIO $ parseThesis path
  modify (\m -> m{thesis=newThesis})
  ok $ toResponse $ fileResponse "touchThesis"

creditsUpload :: Mining Response
creditsUpload = do
  (path, _, _) <- lookFile "creditsFile"
  newCredits <- liftIO $ parseCredits path
  modify (\m -> m{credits=newCredits})
  ok $ toResponse $ fileResponse "touchCredits"

notFoundView :: Html -> Html
notFoundView inner = H.docTypeHtml $ do
  H.head $ do
    H.title "Not found"
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport"    ! A.content "width=device-width, initial-scale=1.0"
    H.meta ! A.name "description" ! A.content ""
    H.meta ! A.name "author"      ! A.content ""
    H.link ! A.href "/static/bootstrap/css/bootstrap.css"            ! A.rel "stylesheet"
    H.link ! A.href "/static/css/style.css"                          ! A.rel "stylesheet"
    H.link ! A.href "/static/bootstrap/css/bootstrap-responsive.css" ! A.rel "stylesheet"
    H.link ! A.href "http://code.jquery.com/ui/1.10.2/themes/smoothness/jquery-ui.css" ! A.rel "stylesheet"
    H.script ! A.type_ "text/html" ! A.id "student-template" $
        H.div mempty
  H.body $ do
    H.h1 "404 Not found"
    H.div ! A.class_ "span9 well" $
      inner

uploadForm :: AttributeValue -> Text -> AttributeValue -> Html
uploadForm name title action = let
  iframeName = ("iframe" `mappend` name)
  in H.div ! A.class_ "fluid-row" ! A.style "display: none" $
      H.div ! A.class_ "span3" $ do
        H.h3 $ H.toHtml title
        H.iframe ! A.name iframeName ! A.style "display: none" $ mempty
        H.form ! A.action action ! A.target iframeName ! A.class_ "form-inline" ! A.enctype "multipart/form-data" ! A.method "post" $ do
          H.input ! A.type_ "file" ! A.name name
          H.input ! A.type_ "submit" ! A.class_ "btn" ! A.value "Update"

mainView :: Map StudentID Student -> Html
mainView students = H.docTypeHtml $ do
  H.head $ do
    H.title title
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport"    ! A.content "width=device-width, initial-scale=1.0"
    H.meta ! A.name "description" ! A.content ""
    H.meta ! A.name "author"      ! A.content ""
    H.link ! A.href "/static/bootstrap/css/bootstrap.css"            ! A.rel "stylesheet"
    H.link ! A.href "/static/css/style.css"                          ! A.rel "stylesheet"
    H.link ! A.href "/static/bootstrap/css/bootstrap-responsive.css" ! A.rel "stylesheet"
    H.link ! A.href "http://code.jquery.com/ui/1.10.2/themes/smoothness/jquery-ui.css" ! A.rel "stylesheet"
    H.script ! A.type_ "application/javascript" $
      H.toHtml $ "var studentData = " `T.append` (E.decodeUtf8 $ encode $ map DatatableStudent $ M.elems students)
    H.script ! A.type_ "application/javascript" ! A.src "/static/jquery/jquery-1.9.1.min.js" $ mempty
    H.script ! A.type_ "application/javascript" ! A.src "/static/bootstrap/js/bootstrap.js" $ mempty
    H.script ! A.type_ "application/javascript" ! A.src "http://code.jquery.com/ui/1.10.2/jquery-ui.js" $ mempty
    H.script ! A.type_ "application/javascript" ! A.src "/static/bacon/js/Bacon.js" $ mempty
    H.script ! A.type_ "application/javascript" ! A.src "/static/mustache/mustache.js" $ mempty
    H.script ! A.type_ "application/javascript" ! A.src "http://datatables.net/download/build/jquery.dataTables.nightly.js" $ mempty
    H.script ! A.type_ "application/javascript" ! A.src "/static/js/doThings.js" $ mempty
    H.script ! A.type_ "text/html" ! A.id "studentModalTemplate" $
      H.div ! A.class_ "fluid-row" $ do
        H.div ! A.class_ "span4" $ do
          H.h2 $ "{{name}}"
          H.hr
          H.p $ "{{degree}}"
          H.p $ "{{major}}"
          H.p $ "{{date}}"
          H.p $ "{{studentId}}"
          H.p $ "{{studentPoints}}"
  H.body $ do
    H.div ! A.id "infoModal" ! A.class_ "modal hide fade" $ do
      H.div ! A.class_ "modal-header" ! A.id "infoModalHeader" $ do
        H.p "modal header content"
      H.div ! A.class_ "modal-body" ! A.id "infoModalBody" $ do
        H.p "modal body"
      H.div ! A.class_ "modal-footer" ! A.id "infoModalFooter" $ do
        H.p "modal footer"
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
    studentModal
    H.div ! A.class_ "container-fluid" $ do
      H.div ! A.class_ "row-fluid" $ do
        H.div ! A.class_ "span8" $ do
          H.div ! A.class_ "span3" $
            H.div ! A.class_ "well sidebar-nav" $
              H.ul ! A.class_ "nav nav-list" $ do
                H.li ! A.class_ "nav-header" $ "Sidebar"
                H.li $ H.a ! data_target "students" ! A.href "#" $ "Students"
                H.li $ H.a ! data_target "degrees" ! A.href "#" $ "Degreees"
                H.li $ H.a ! data_target "credits" ! A.href "#" $ "Credits"
          H.div ! A.class_ "span4" $ do
            uploadForm "studentFile" "Update students file" "student/upload"
            uploadForm "thesisFile" "Update thesis file" "thesis/upload"
            uploadForm "creditsFile" "Update credits file" "credits/upload"
    H.div ! A.class_ "span9" $ do
      H.div ! A.id "userData" ! A.class_ "hero-unit" $ do
        H.table ! A.class_ "databox" $ mempty
      H.div ! A.id "degreeData" ! A.style "display: none" ! A.class_ "hero-unit" $ do
        H.table ! A.class_ "databox" $ mempty
      H.div ! A.id "courseData" ! A.style "display: none" ! A.class_ "hero-unit" $ do
        H.table ! A.class_ "databox" $ mempty
  where
    data_toggle = attribute "data-toggle" " data-toggle=\""
    data_target = attribute "data-target" " data-target=\""
    title = "Käyttöliittymät harkka"

lookBSsafe :: (Monad m, Functor m, HasRqData m) => String -> m (Maybe ByteString)
lookBSsafe key = listToMaybe <$> lookBSs key

looksafe :: (Monad m, Functor m, HasRqData m) => String -> m (Maybe String)
looksafe key = listToMaybe <$> looks key

studentsCredits ::  Student -> [Credit] -> [Credit]
studentsCredits student credits =
  filter (\c -> studentId student == creditStudentId c) credits

main :: IO ()
main = do
  thesis <- parseThesis "data/kandit.txt"
  students <- parseStudents "data/opiskelijat.txt"
  credits <- parseCredits "data/suoritukset.txt"
  let state = MinedData students thesis credits
  simpleHTTP nullConf{port=25565} $ do
    decodeBody (defaultBodyPolicy "/tmp" 16384 16384 16384)
    evalStateT (msum [
        nullDir >> ok (toResponse $ mainView students)
      , dir "student" $ studentQuery
      , dirs "student/upload" $ studentsUpload
      , dirs "student/data" $ studentsData
      , dirs "degree/data" $ thesisData
      , dirs "course/data" $ do { nullDir; creditsData }
      , dirs "course/data/union" $ unionCoursesData
      , dirs "thesis/upload" $ thesisUpload
      , dir "static" $ serveDirectory EnableBrowsing [] "public/"
      ]) state

