module Messages
  ( encode
  , decode
  )
where

import Control.Monad (ap)
import Text.Parsec
import qualified Text.Parsec.Char as Pc
import Data.List (intercalate)
import qualified Data.UUID as UUID
import Models

encode :: Payload -> String
encode (Msg id msg) = intercalate "|" ["message", show id, msg]
encode (Handshake id) = intercalate "|" ["handshake", show id, ""]

decode :: String -> Maybe Payload
decode msg =
    case parse messageParser "" msg of
      Left _ -> Nothing
      Right m -> m

messageParser :: Parsec String () (Maybe Payload)
messageParser =
    messagePayload <|> handshakePayload

messagePayload :: Parsec String () (Maybe Payload)
messagePayload =
    let
      uuid = (try $ string "message") >> pipeParser >> uuidParser
      msg = fmap Just $ pipeParser >> manyTill Pc.anyChar eof :: Parsec String () (Maybe String)
      uuidFn = fmap (fmap Msg) uuid :: Parsec String () (Maybe (String -> Payload))
    in
      uuidFn >>= (\mf -> fmap (\ms -> mf <*> ms) msg)

handshakePayload :: Parsec String () (Maybe Payload)
handshakePayload =
    fmap (fmap Handshake) $ (try $ string "handshake") >> pipeParser >> uuidParser

uuidParser :: Parsec String () (Maybe UUID.UUID)
uuidParser =
    let
      dashParser = Pc.string "-"
      parsers = [ count 8 hexParser
                , dashParser
                , count 4 hexParser
                , dashParser
                , count 4 hexParser
                , dashParser
                , count 4 hexParser
                , dashParser
                , count 12 hexParser
                ]
    in
      fmap UUID.fromString $
        foldl1 (\result nextParser -> (fmap (++) result)
                                      <*> nextParser)
               parsers


hexParser :: Parsec String () Char
hexParser = Pc.oneOf "0123456789abcdef"

pipeParser :: Parsec String () Char
pipeParser = Pc.char '|'
