module MessagesSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List (intercalate)
import Data.Maybe
import qualified Data.UUID as UUID
import System.Random
import Messages
import Models

instance Arbitrary UUID.UUID where
    arbitrary = (fst . random . mkStdGen) <$> arbitrary

spec :: Spec
spec = do
    let uuid = fromJust $ UUID.fromString "dd7b8e07-10bf-4ea7-9564-0d1de00b363c"
    describe "Encoding a message" $ do
      describe "Message" $ do
        it "prepends the message type and the server id" $ do
          encode (Msg uuid "hi, what's up?") `shouldBe`
            "message|dd7b8e07-10bf-4ea7-9564-0d1de00b363c|hi, what's up?"
      describe "Handshake" $ do
        it "prepends the handshake type and server id, with an empty message" $ do
          encode (Handshake uuid) `shouldBe`
            "handshake|dd7b8e07-10bf-4ea7-9564-0d1de00b363c|"
    describe "Decoding a message" $ do
      describe "Message" $ do
        it "returns a Msg with the uuid and message" $ do
          decode "message|dd7b8e07-10bf-4ea7-9564-0d1de00b363c|hi, what's up?" `shouldBe`
            (Just (Msg uuid "hi, what's up?"))
        it "returns nothing if the uuid cannot be parsed" $ do
          decode "message|imnotauuid|hi, what's up?" `shouldBe` Nothing
      describe "Handshake" $ do
        it "returns a Handshake with the uuid" $ do
          decode "handshake|dd7b8e07-10bf-4ea7-9564-0d1de00b363c|" `shouldBe`
            (Just (Handshake uuid))
        it "returns nothing if the uuid cannot be parsed" $ do
          decode "handshake|imnotauuid|" `shouldBe` Nothing
      describe "wrong message" $ do
        it "returns nothing if the message prefix is not recognized" $ do
          decode "finger|dd7b8e07-10bf-4ea7-9564-0d1de00b363c|hi, what's up?" `shouldBe` Nothing
      describe "symmetry" $ do
        it "decodes its own encoding of a message" $ property $
          \u m -> (decode . encode) (Msg u m) == Just (Msg u m)
        it "decodes its own encoding of a handshake" $ property $
          \u -> (decode . encode) (Handshake u) == Just (Handshake u)
