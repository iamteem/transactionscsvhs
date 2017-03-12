{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Transactions where

import Data.Ord (comparing)
import Data.Vector.Algorithms.Merge as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Data.Csv
import qualified Data.List as L
import qualified Data.List.Split as S
import Data.Time
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import qualified Data.Text as T
import GHC.Generics
import Text.Printf (printf)

csvPath :: FilePath
csvPath = "data/transactions.csv"

dateFormat :: String
dateFormat = "%m/%d/%y"

data Transaction = Transaction { name :: !T.Text
                               , date :: !Day
                               , amount :: !Float
                               , category :: !T.Text
                               , memo :: !T.Text
                               , vendor :: !T.Text
                               } deriving Generic

instance FromRecord Transaction
instance ToRecord Transaction

instance FromField Day where
  parseField s = parseTimeM True defaultTimeLocale dateFormat $ C.unpack s

instance ToField Day where
  toField d = C.pack $ formatTime defaultTimeLocale dateFormat d

instance Show Transaction where
  show t = mconcat $ L.intersperse " " [show (date t), T.unpack (name t), formatAmount (amount t), T.unpack (category t),
    T.unpack (memo t), T.unpack (vendor t)]

instance Ord Transaction where
  compare t1 t2 =
    case date t1 /= date t2 of
      True -> comparing date t1 t2
      False -> comparing name t1 t2

instance Eq Transaction where
  (==) t1 t2 = (date t1 == date t2) && (name t1 == name t2)

run :: IO ()
run = do
  contents <- B.readFile csvPath
  case (decode HasHeader contents :: Either String (V.Vector Transaction)) of
    Left err -> putStrLn $ "Invalid CSV: " ++ err
    Right transactions -> sortTransactions transactions >>= displayTransactions

formatAmount :: Float -> String
formatAmount amt = if amt >= 0
  then "\t " L.++ formattedAmount
  else "\t-" L.++ formattedAmount
  where formattedAmount = mconcat [commaSeparated, decimalAmt]
        commaSeparated = "$" ++ commafy num
        num = abs amt
        decimalPart = (round (num * 100) :: Integer) - (100 * truncate num)
        decimalAmt = if decimalPart > 0 then ("." ++ printf "%02d" decimalPart) else ".00"

commafy :: Float -> String
commafy f = let s = reverse $ show i
                i = truncate f
                chunks = S.chunksOf 3 s
            in reverse $ L.concat $ L.intersperse "," chunks

displayTransactions :: V.Vector Transaction -> IO ()
displayTransactions transactions = V.mapM_ displayTransaction transactions

displayTransaction :: Transaction -> IO ()
displayTransaction = putStrLn . show

sortTransactions :: V.Vector Transaction -> IO (V.Vector Transaction)
sortTransactions txns = V.thaw txns >>= \mtxns -> (A.sortBy compare mtxns) >> V.freeze mtxns >>= return

-- sortTransactions' :: V.Vector Transaction -> IO (V.Vector Transaction)
-- sortTransactions' txns = let fun t1 t2 = comparing date t1 t2
--                         in do
--                             kamote <- V.thaw txns
--                             A.sortBy fun kamote
--                             j <- V.freeze kamote
--                             return j
