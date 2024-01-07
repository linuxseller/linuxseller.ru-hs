{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, pack)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import System.IO.Unsafe
import GHC.IO (unsafePerformIO)
import Data.List

import Import
{-----------------------
import DB (getStory, getStroriesList, Story, StoryId)
import Handlers
-----------------------}
type StoryId = Int
type Login = String
type StoryName = String
type StoryText = String
type StoryDateStamp = String
data Story = Story StoryId Login StoryName StoryText StoryDateStamp deriving Show
type Stories = [Story]
instance FromRow Story where
    fromRow = Story <$> field <*> field <*> field <*> field <*> field

getStroriesList :: IO Stories
getStroriesList = do
    conn <- open "test.db"
    r <- query_ conn (Query $ pack "SELECT * FROM stories") :: IO Stories
    close conn
    return r

getStory id = do
    conn <- open "test.db"
    r <- query_ conn ( Query $ pack ("SELECT * FROM stories WHERE StoryId=="<>show id)) :: IO Stories
    close conn
    return $ Just $ head r

data MyRoute = Home

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/home"


data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/                 HomeR             GET
/stories          StoriesR          GET
/stories/#StoryId StoriesShowStoryR GET
|]

instance Yesod HelloWorld
getHomeR :: Handler Html
getHomeR = getStoriesR

{-# NOINLINE getStoriesR #-}
getStoriesR :: Handler Html
getStoriesR = defaultLayout $ do
    setTitle "Stories Index"
    $(widgetFile "stories")
    where stories = unsafePerformIO getStroriesList

getStoriesShowStoryR :: Int -> Handler Html
getStoriesShowStoryR id = defaultLayout $ do
    setTitle "Story"
    $(widgetFile "story")
    where story = unsafePerformIO $ getStory id

main :: IO ()
main = warp 3000 HelloWorld
