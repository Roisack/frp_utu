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
import qualified Data.ByteString.Char8 as B

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
newtype DatatableStudent = DatatableStudent Student

instance ToJSON DatatableStudent where
  toJSON (DatatableStudent student) = toJSON [
        studentId student
      , name student
      , degree student
      , major student
      , T.pack $ show $ studentPoints student
      , T.pack $ drop 5 $ show $ date student
    ]

instance ToMessage Value where
  toMessage x = encode x
  toContentType _ = B.pack ("application/json" :: String)

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

userModal :: Html
userModal = H.div ! A.id "modal_user" ! A.class_ "modal hide fade" $ do
  H.div ! A.class_ "modal-header" $ do
    H.button ! A.type_ "button" ! A.class_ "close" ! data_dismiss "modal" ! aria_hidden "true" $
      "x"
    H.h3 ! A.id "user_header" $ mempty
  H.div ! A.class_ "modal-body" ! A.id "user_body" $ do
    mempty
  H.div ! A.class_ "modal-footer" $ do
    H.a ! A.href "#" ! A.class_ "btn close" $ "Close"
  where
    data_dismiss = attribute "data-dismiss" "data-dismiss=\""
    aria_hidden = attribute "aria-hidden" "aria-hidden=\""

mainView :: [Student] -> Html
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
    H.toHtml $ "var studentData = " `T.append` (E.decodeUtf8 $ encode $ map DatatableStudent students)
  H.script ! A.type_ "application/javascript" ! A.src "/static/jquery/jquery-1.9.1.min.js" $ mempty
  H.script ! A.type_ "application/javascript" ! A.src "/static/bootstrap/js/bootstrap.js" $ mempty
  H.script ! A.type_ "application/javascript" ! A.src "http://code.jquery.com/ui/1.10.2/jquery-ui.js" $ mempty
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
    userModal
    H.div ! A.class_ "container-fluid" $
      H.div ! A.class_ "row-fluid" $
        H.div ! A.class_ "span3" $
          H.div ! A.class_ "well sidebar-nav" $
            H.ul ! A.class_ "nav nav-list" $ do
              H.li ! A.class_ "nav-header" $ "Sidebar"
              H.li $ H.a ! A.href "#" $ "Students"
              H.li $ H.a ! A.href "#" $ "Degreees"
              H.li $ H.a ! A.href "#" $ "Courses"
    H.div ! A.class_ "span9" $ do
      H.div ! A.class_ "hero-unit" $ do
        H.table ! A.id "databox" $ mempty
  where
    data_toggle = attribute "data-toggle" "data-toggle=\""
    data_target = attribute "data-target" "data-target=\""
    title = "Käyttöliittymät harkka"

lookBSsafe :: (Monad m, Functor m, HasRqData m) => String -> m (Maybe ByteString)
lookBSsafe key = listToMaybe <$> lookBSs key

main :: IO ()
main = do
  thesis <- parseThesis "data/kandit.txt"
  students <- parseStudents "data/opiskelijat.txt"
  simpleHTTP nullConf{port=25565} $ do
    decodeBody (defaultBodyPolicy "/tmp" 4096 4096 4096)
    msum [
        nullDir >> ok (toResponse $ mainView students)
      , dir "static" $ serveDirectory EnableBrowsing [] "public/"
      ]

