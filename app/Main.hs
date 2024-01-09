{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, pack, unpack)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Web.ClientSession   as CS

import System.IO.Unsafe
import GHC.IO (unsafePerformIO)
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import Import
{-----------------------
import DB (getStory, getStroriesList, Story, StoryId)
import Handlers
-----------------------}

type UserId = Int
type Login = String
type Password = String
type Email = String
type CreationDate = String
data User = User UserId Login Password Email CreationDate
defaultUser = User (-1) "" "" "" ""
type Users = [User]
instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field

type StoryId = Int
type StoryName = String
type StoryText = String
type StoryDateStamp = String
data Story = Story StoryId Login StoryName StoryText StoryDateStamp deriving Show
type Stories = [Story]
instance FromRow Story where
    fromRow = Story <$> field <*> field <*> field <*> field <*> field

db :: FilePath
db = "test.db"

getUsersList :: IO Users
getUsersList = do
    conn <- open db
    r <- query_ conn (Query $ pack "SELECT * FROM users") :: IO Users
    close conn
    return r

getStoriesList :: IO Stories
getStoriesList = do
    conn <- open db
    r <- query_ conn (Query $ pack "SELECT * FROM stories") :: IO Stories
    close conn
    return r

getStory id = do
    conn <- open db
    r <- query_ conn ( Query $ pack ("SELECT * FROM stories WHERE StoryId=="<>show id)) :: IO Stories
    close conn
    return $ Just $ head r

getUser login = do
    conn <- open db
    r <- query_ conn ( Query $ pack ("SELECT * FROM users WHERE login='"<>login<>"'")) :: IO Users
    close conn
    return $ Just $ head r

getUserPassword :: User -> Password
getUserPassword (User _ _ p _ _) = p

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/                 HomeShowR         GET
/stories          StoriesShowR      GET
/stories/#StoryId StoriesShowStoryR GET
/login            LoginShowR        GET POST
|]


instance Yesod HelloWorld

instance RenderMessage HelloWorld FormMessage where
    renderMessage _ _ = defaultFormMessage

{-# NOINLINE getLoginShowR #-}
getLoginShowR :: Handler Html
getLoginShowR = defaultLayout $ do
    setTitle "Login"
    $(widgetFile "login")

{-# NOINLINE postLoginShowR #-}
postLoginShowR :: Handler ()
postLoginShowR = do
    (login, passwordd) <- runInputPost $ (,) <$> ireq textField "login" <*> iopt textField "password"
    let dbpassword = getUserPassword $ fromMaybe defaultUser $ unsafePerformIO $ getUser $ unpack login
    case passwordd of
        Nothing -> redirect LoginShowR
        Just password -> if unpack password==dbpassword then setSession "login" login else redirect LoginShowR
    redirect HomeShowR

getHomeShowR :: Handler Html
getHomeShowR = getStoriesShowR

{-# NOINLINE getStoriesShowR #-}
getStoriesShowR :: Handler Html
getStoriesShowR = defaultLayout $ do
    sess <- getSession
    setTitle "Stories Index"
    $(widgetFile "stories")
    where stories = unsafePerformIO getStoriesList

getStoriesShowStoryR :: Int -> Handler Html
getStoriesShowStoryR id = defaultLayout $ do
    setTitle "Story"
    $(widgetFile "story")
    where story = unsafePerformIO $ getStory id

main :: IO ()
main = warp 3000 HelloWorld
