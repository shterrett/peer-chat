module CommandsSpec where

import Test.Hspec
import Network
import Text.Parsec (parse)
import Commands
import Models

spec :: Spec
spec = do
    describe "Parsing Commands" $ do
      describe "Switching between connections" $ do
        it "returns a switch command with a connection name for switch" $ do
          command "/switch friend" `shouldBe` (Right (Switch "friend"))
        it "returns a switch command with a connection name for s" $ do
          command "/s friend" `shouldBe` (Right (Switch "friend"))
        it "fails if there is no connection name present" $ do
          command "/switch" `shouldBe` (Left "Expecting Name")
      describe "Removing a connection" $ do
        it "returns a remove command with a connection name for remove" $ do
          command "/remove friend" `shouldBe` (Right (Remove "friend"))
        it "returns a remove command with a connection name for r" $ do
          command "/r friend" `shouldBe` (Right (Remove "friend"))
        it "fails if there is no connection name present" $ do
          command "/remove" `shouldBe` (Left "Expecting Name")
      describe "Quitting" $ do
        it "returns a quit command from quit" $ do
          command "/quit" `shouldBe` (Right Quit)
        it "returns a quit command from q" $ do
          command "/q" `shouldBe` (Right Quit)
      describe "Creating a new connection" $ do
        it "parses an ip address" $ do
          parse ipParser "" "192.168.2.42" `shouldBe` (Right "192.168.2.42")
        it "parses a port" $ do
          parse portParser ""  ":12345" `shouldBe` (Right (PortNumber 12345))

        it "creates a connection when given the required information" $ do
          command "/connect friend 192.168.2.42:12345"
            `shouldBe`
            (Right (Add $ Connection { name = "friend"
                             , addr = "192.168.2.42"
                             , port = PortNumber 12345
                             }))
        it "creates a connection with the default port" $ do
          command "/connect friend 192.168.2.42"
            `shouldBe`
            (Right (Add $ Connection { name = "friend"
                             , addr = "192.168.2.42"
                             , port = defaultPort
                             }))
        it "creates a connection with c" $ do
          command "/c friend 192.168.2.42:12345"
            `shouldBe`
            (Right (Add $ Connection { name = "friend"
                             , addr = "192.168.2.42"
                             , port = PortNumber 12345
                             }))
      describe "Unrecognized command" $ do
        it "returns an error for an unrecognized command" $ do
          command "/launch-missles"
            `shouldBe`
            (Left "Unrecognized Command. Must be one of connect, switch, remove, quit")
      describe "Message" $ do
        it "accepts any string as a message" $ do
          command "hello, world!" `shouldBe` (Right (Message "hello, world!"))
