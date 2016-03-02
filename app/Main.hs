{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


module Main where

import Lib
import Data.Aeson
import Data.Text
import Data.List
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Maybe


data Card = Card { name :: String,
                   manaCost :: Int,
                   attack :: Maybe Int,
                   life :: Maybe Int,
                   cardType :: String,
                   damage :: Maybe Int
                  } deriving (Show,Generic)

type Deck = [Card]
type Collection = [Card]

jsonFile :: FilePath
jsonFile = "data/cards.json"

instance FromJSON Card
instance ToJSON Card

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

baseScore :: (Maybe Int, Maybe Int, Maybe Int ,Int) -> Int
baseScore (Just damage, Nothing, Nothing, manaCost) = damage - manaCost
baseScore (Nothing, Just attack, Just life, manaCost) = (attack + life) - 2*manaCost

evaluateCard :: Card -> Int
evaluateCard card = baseScore(damage card, attack card, life card, manaCost card)

sortCard :: Card -> Card -> Ordering
sortCard c1 c2
  | evaluateCard c1 < evaluateCard c2 = GT
  | evaluateCard c1 > evaluateCard c2 = LT
  | evaluateCard c1 == evaluateCard c2 = EQ

buildDeck :: (Collection, Deck) -> Deck
buildDeck ([], _) = []
buildDeck (collection, deck) = Data.List.take 20 (sortBy sortCard collection)

selectGreaterThanFour collection = collection

main :: IO ()
main = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> getJSON) :: IO (Either String [Card])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right collection -> print (buildDeck (collection, []))

