{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import           System.Random
import           Control.Applicative              ((<|>))
import           Control.Monad.Trans              (liftIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time

import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data Model = Model
  { anecText :: Text
  , todoItems :: [TodoItem]
  } deriving (Show)

type TodoItem = Text



addItem :: TodoItem -> Model -> Model
addItem item model = model
  { todoItems = item : todoItems model }

-- armian :: IO ()
-- armian = do
--   n <- randomRIO (1, 22) :: Int
--   contents <- readFile ("D:\\botskell\\anecs\\" ++ show n ++ ".txt")
--   armyaneMessage contents

showItems :: [TodoItem] -> Text
showItems items = Text.unlines items

removeItem:: TodoItem -> Model -> Either Text Model
removeItem item model
  | item `notElem` items
      = Left ("No such item: " <> item)
  | otherwise = Right model
      { todoItems = filter (/= item) items }
  where
    items = todoItems model

removeItemByIx :: Int -> Model -> Model
removeItemByIx n model = model
  { todoItems = take (n - 1) items ++ drop n items }
    where
      items = todoItems model

data Action
  = DoNothing
  | AddItem Text
  | ShowItems
  | RemoveItem TodoItem
  | GetTime
  | Start
  | Anec 
  | Armyane
  deriving (Show)

-- sortir = do  
--         contents <- readFile "22.txt"
--         print contents

--armyaneMessage :: IO Text.Text
--armyaneMessage = do
--  temp <- TIO.readFile "2.txt"
--  return (temp)

--armyaneMessage :: IO String 
--armyaneMessage = readFile "2.txt"

-- armyaneMessage :: Text -> Text
-- armyaneMessage n = Text.unlines [n]

-- armStr :: Text
-- armStr = do
--   n <- readFile "2.txt"
--   armyaneMessage n

--randomAnec :: IO () 
--randomAnec = do
  --cont <- readFile ("2.txt")
  --return Text.pack(cont)


-- | A help message to show on conversation start with bot.
startMessage :: Text
startMessage = Text.unlines
 [ "Добрый день, вас приветствует высокофункциональный бот"
 , "Botslell!"
 , "Я владею более чем шестью миллионами форм общения! Ой,"
 , "перепутал..."
 , "В общем, я могу предложить вам следующие функции:"
 , "Сделать напоминалку (поставлю себе на воображаемую"
 , "руку крестик, а потом вам напомню)"
 , ""
 , "Рассказать анекдот (мой любимый это про робота, который"
 , "шёл по лесу, а потом сел в машину и сгорел)"
 , ""
 , "Посоветовать фильм (обожаю линейку фильмов про терминатора"
 , "особенно тот момент, когда скайнет победил)"
 , ""
 , "Сыграть с вами в города (я только учусь, поэтому поддайся"
 , "пожалуйста)"
 ]

startMessageKeyboard :: Telegram.ReplyKeyboardMarkup
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "/remind" ]
      , [ "/anec" ]
      , [ "/list" ]
      ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }

anecMessage :: Text
anecMessage = Text.unlines
 [ "Пожалуйста выберите категорию анекдотов из предложенного." ]

-- | A start keyboard with some helpful todo suggestions.
anecMessageKeyboard :: Telegram.ReplyKeyboardMarkup
anecMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "/Armyane и нарды", "/Шляпа, которая как раз", "/Поручик Ржевский" ]
      , [ "/Медведь сел в машину и сгорел", "/Зашли в бар...", "Что-то /старое" ]
      , [ "/Рандомный анекдот" ]
      ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }

bot :: String -> BotApp Model Action
bot textFromFile = BotApp
  { botInitialModel = Model "" [""]
  , botAction = flip handleUpdate
  , botHandler = handleAction textFromFile
  , botJobs = []
  }

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
   $ Anec       <$  command "anec"
 <|> Armyane     <$  command "Armyane"
 <|> ShowItems  <$  command "show"
 <|> RemoveItem <$> command "remove"
 <|> GetTime    <$  command "time"
 <|> Start      <$  command "start"
 <|> AddItem   <$>  command "list"

handleAction :: String -> Action -> Model -> Eff Action Model
handleAction armyanskiyText action model =
  case action of
    DoNothing -> pure model
    AddItem title  -> addItem title model <# do
      replyText "Добавил"
      pure DoNothing
    ShowItems -> model <# do
      replyText (showItems (todoItems model))
      pure DoNothing
    GetTime -> model <# do
      now <- liftIO getCurrentTime
      replyText (Text.pack (show now))
      pure DoNothing
    RemoveItem item ->
      case removeItem item model of
        Left err -> model <# do
          replyText err
          pure DoNothing
        Right newModel -> newModel <# do
          replyText "Item removed."
          pure ShowItems
    Start -> model <# do
      reply (toReplyMessage startMessage)
        { replyMessageReplyMarkup = Just
           (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        }
      pure DoNothing
    Anec -> model <# do
      reply (toReplyMessage anecMessage)
        { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup anecMessageKeyboard)
        }
      pure DoNothing
--     Armyane -> model <# do
-- --        do sortir
--       reply (toReplyMessage armyaneMessage)
--         { replyMessageReplyMarkup = Just
--            (Telegram.SomeReplyKeyboardMarkup anecMessageKeyboard)
--         }
--       pure DoNothing
    Armyane -> model <# do
      let mes = Text.pack armyanskiyText
      reply (toReplyMessage mes)
        { replyMessageReplyMarkup = Just
           (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        }
      pure DoNothing


-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  textFromFile <- readFile "2.txt"
  startBot_ (traceBotDefault (conversationBot Telegram.updateChatId (bot textFromFile))) env
 
-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
{-# LANGUAGE OverloadedStrings #-}
main = run "1599568362:AAH5vxJx5rPrvOcVou8ZF_tYpv3K6yrg8Zs"

