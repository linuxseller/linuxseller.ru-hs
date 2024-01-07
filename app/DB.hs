module DB (
 StoryId,
 Login,
 StoryName,
 StoryText,
 StoryDateStamp,
 Story,
 Stories,
 getStory,
 getStroriesList
)where

import Data.Text (Text, pack)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

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

