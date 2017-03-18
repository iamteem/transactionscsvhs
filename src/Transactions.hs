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
import qualified Control.Monad as M
import Control.Monad.IO.Class

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


type TransactionsVector = V.Vector Transaction

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
run = parseTransactions >>= sortTransactions >>= displayTransactions -- sortTransactions transactions >>= displayTransactions

parseTransactions :: IO TransactionsVector
parseTransactions = do
  contents <- B.readFile csvPath
  case (decode HasHeader contents :: Either String TransactionsVector) of
    Left err -> (putStrLn "Invalid CSV: ") >> return V.empty
    Right transactions -> return transactions

formatAmount :: Float -> String
formatAmount amt = if amt >= 0
  then formattedAmount
  else "-" ++ formattedAmount
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

displayTransactions :: TransactionsVector -> IO ()
displayTransactions transactions = V.mapM_ displayTransaction transactions

displayTransaction :: Transaction -> IO ()
displayTransaction = putStrLn . show

sortTransactions :: TransactionsVector -> IO TransactionsVector
sortTransactions txns = V.thaw txns >>= \mtxns -> (A.sortBy compare mtxns) >> V.freeze mtxns >>= return

-- sortTransactions'' :: V.Vector Transaction -> IO (V.Vector Transaction)
-- sortTransactions'' txns = let fun t1 t2 = comparing date t1 t2
--                           in do
--                              kamote <- V.thaw txns
--                              A.sortBy fun kamote
--                              j <- V.freeze kamote
--                              return j

summary :: IO ()
summary = creditReport >> debitReport >> netReport

creditReport :: IO ()
creditReport = do
  total <- creditTotal
  putStrLn $ "Credit: " ++ formatAmount total
  return ()

debitReport :: IO ()
debitReport = do
  total <- debitTotal
  putStrLn $ "Debit: " ++ formatAmount total
  return ()

netReport :: IO ()
netReport = do
  total <- net
  putStrLn $ "Net: " ++ formatAmount total
  return ()

creditTotal :: IO Float
creditTotal = parseTransactions >>= getTotal (Just (<0)) >>= \amt -> return $ abs amt

debitTotal :: IO Float
debitTotal = parseTransactions >>= getTotal (Just (>0)) >>= \amt -> return $ abs amt

getTotal :: Maybe (Float -> Bool) -> TransactionsVector -> IO Float
getTotal Nothing txns = return $ sum $ V.map amount txns
getTotal (Just fun) txns = return $ sum $ V.filter fun $ V.map amount txns

net :: IO Float
net = parseTransactions >>= getTotal Nothing
