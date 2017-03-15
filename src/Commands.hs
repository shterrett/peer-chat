module Commands where

import Models
import Text.Parsec
import Text.Parsec.Char as Pc
import Text.Parsec.Error
import Data.List (intercalate)
import Control.Arrow
import Network

data Command =
    Message String
    | Add Connection
    | Switch ConnectionName
    | Remove ConnectionName
    | Quit
    deriving (Eq, Show)

command :: String -> Either String Command
command s =
    case parse commandParser "" s of
      Left e -> Left $ showError e
      Right cmd -> Right cmd
      where expectMsg m =
              case m of
                Expect _ -> True
                _ -> False
            showError =
              (intercalate "; ") .
                 (fmap messageString) .
                 (filter expectMsg) .
                 errorMessages

commandParser :: Parsec String () Command
commandParser =
    let unrecognizedCommand = "Unrecognized Command. Must be one of connect, switch, remove, quit"
    in
      (commandPrefix >>
      (quitParser
      <|> switchParser
      <|> removeParser
      <|> connectParser <?> unrecognizedCommand))
      <|> messageParser

quitParser :: Parsec String () Command
quitParser =
    let long = try $ string "quit"
        short = try $ string "q"
    in
      fmap (\_ -> Quit) $ (long <|> short)
                          >> eof

switchParser :: Parsec String () Command
switchParser =
    let long = try $ string "switch"
        short = try $ string "s"
        error = "Expecting Name"
    in
      fmap Switch $ (long <|> short)
                    >> (Pc.space <?> error)
                    >> (manyTill Pc.alphaNum eof <?> error)

removeParser :: Parsec String () Command
removeParser =
    let long = try $ string "remove"
        short = try $ string "r"
        error = "Expecting Name"
    in
      fmap Remove $ (long <|> short)
                    >> (Pc.space <?> error)
                    >> (manyTill Pc.alphaNum eof <?> error)

connectParser :: Parsec String () Command
connectParser =
    let long = try $ string "connect"
        short = try $ string "c"
        error = "Expecting name ip_address:port"
        name = manyTill Pc.alphaNum Pc.space
    in
      fmap Add $ (fmap Connection $ (long <|> short)
                        >> (Pc.space <?> error)
                        >> (name <?> error))
                        <*> (ipParser <?> error)
                        <*> (portParser <?> error)

ipParser :: Parsec String () String
ipParser = fmap (intercalate ".") $ sepBy1 (many Pc.digit) (Pc.char '.')

portParser :: Parsec String () PortID
portParser =
    let port = Pc.char ':'
                  >> fmap (\p -> PortNumber (read p))
                          (many Pc.digit)
    in option defaultPort port

commandPrefix :: Parsec String () String
commandPrefix = string "/"

messageParser :: Parsec String () Command
messageParser = fmap Commands.Message $ manyTill Pc.anyChar eof
