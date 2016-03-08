{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


module Main where

import Lib
import Data.Aeson
import Data.List
import Debug.Trace
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
type CardWithCost = (Int, Card)
type ZippedCurve = [(Int, [Card])]
data AvailableMana = AvailableMana Int deriving (Ord, Eq, Show)
manaCurve = [1,2,3,4,5,6,7,8,9,10]
manaCurveQuantities = [4,6,6,4,4,4,0,0,0,2]

type Collection = [Card]

jsonFile :: FilePath
jsonFile = "data/cards.json"

instance FromJSON Card
instance ToJSON Card

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

baseScore :: (Maybe Int, Maybe Int, Maybe Int ,Int) -> Int
baseScore (Just damage, Nothing, Nothing, manaCost) = damage
baseScore (Nothing, Just attack, Just life, manaCost) = (attack + life)

evaluateCard :: Int -> Card -> Int
evaluateCard mana card =
  if mana >= manaCost card
    then baseScore (damage card, attack card, life card, manaCost card)
    else -100

withMana :: Int -> Card -> CardWithCost
withMana mana card = (mana, card)

sortCollectionForMana :: Int -> Collection -> [CardWithCost]
sortCollectionForMana mana collection = sortBy sortCard (map (withMana mana) collection)

sortCard :: CardWithCost -> CardWithCost -> Ordering
sortCard (m1,c1) (m2,c2)
  | evaluateCard m1 c1 < evaluateCard m2 c2 = GT
  | evaluateCard m1 c1 > evaluateCard m2 c2 = LT
  | evaluateCard m1 c1 == evaluateCard m2 c2 = EQ

createCurve :: Collection -> [[Card]]
createCurve collection =  map (removeManaCost) [ sortCollectionForMana mana collection | mana <- manaCurve ]

removeManaCost :: [CardWithCost] -> [Card]
removeManaCost list = map (snd) list

zipCurve :: [[Card]] -> ZippedCurve
zipCurve curve = zip manaCurveQuantities curve

applyCurve :: ZippedCurve -> Deck
applyCurve list = concat [ take quantity cards | (quantity, cards) <- list]

buildDeck :: Collection -> Deck
buildDeck [] = []
buildDeck collection = applyCurve (zipCurve (createCurve collection))


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
  Right collection -> print (map name (buildDeck collection))

