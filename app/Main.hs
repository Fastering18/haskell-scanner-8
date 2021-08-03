{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main where

import Control.Monad (when, forM_, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = pingpongExample

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do 
    userFacingError <- runDiscord $ def { 
                                        discordToken = "Bot ODY3NTcxMTA0Nzg0MzE4NDY0.YPjCjg.LZc97Q06ZtLJFLZvH62oDOuYY_E"
                                        , discordOnEvent = eventHandler 
                                        }
    TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
               _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
               --threadDelay (4 * 10^6)
               _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
               pure ()
       _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower
