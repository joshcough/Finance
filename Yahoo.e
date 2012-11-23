module HW1 where

import Prelude
import Function
import Control.Ap
import Control.Functor
import Control.Traversable
import IO
import IO.CSV
import List
import Field
import Tuple
import Maybe
import Native.Tuple
import Parse
import Date
import File
import String as String
import Layout.Report
import Environment.Home
import Syntax.IO
import Layout.Format as Fmt
import Relation.Op as Op

-- ======= Potential library functions ========= --

foreign
  function "com.clarifi.reporting.util.IOUtils" "dump" dump : a -> a

concatStrings : List (String) -> String
concatStrings = foldl (++_String) ""

infixr 5 <++>
--<++> : c <- (a,b) => Maybe {..a} -> Maybe {..b} -> Maybe {..c}
(<++>) a b = liftA2 maybeAp (++) a b
parseField : (String -> Maybe a) -> Field b a -> String -> Maybe {..b}
parseField parseFunction fld = (fmap maybeFunctor $ v -> {fld = v}) . parseFunction

-- convert YYYY-MM-DD to MM/DD/YYYY
pDate fld s = parseField parseDate fld $ reform (split_String '-' s) where
  reform (yyyy :: mm :: dd :: []) = concatStrings [mm, "/", dd, "/", yyyy] 
pDouble     = parseField parseDouble
pInt        = parseField (parseInt 10)

readDataFile tupReader = readData tupReader . readCSVFile
readDataURL  tupReader = readData tupReader . readCSVURL

readData tupReader fetchContents = (readDataTups . reverse . drop 1) <$> fetchContents where
  readDataTups ls = fmap maybeFunctor relation $ 
    sequence listTraversable maybeMonad (fmap listFunctor tupReader ls) 

go = display . (maybe $ text "parse error") where display f i = i >>= (javaFX . f)

-- ========= Yahoo Code ========= --


pad2 s = if (length_String s == 1) ("0" ++_String s) s
yahooBaseURL = "http://ichart.finance.yahoo.com/table.csv?"
yahooDate a b c d = concatStrings [  
  "&", a, "=", pad2 $ getMonth d |> toString, 
  "&", b, "=", pad2 $ getDate  d |> toString,
  "&", c, "=", (getYear d) + 1900 |> toString
]
yahooStartDate = yahooDate "a" "b" "c"
yahooEndDate   = yahooDate "d" "e" "f"
yahooURL sym startDate endDate =
  concatStrings [yahooBaseURL, "s=", sym, (yahooStartDate startDate), (yahooEndDate endDate), "&g=d&ignore=.csv"]
-- TODO: add flag for daily, monthly, yearly, etc
yahooString sym startDate endDate = readURL $ yahooURL sym startDate endDate
yahoo sym startDate endDate = readHistoryFromURL $ yahooURL sym startDate endDate


-- ========= Classwork below ========== --

field datefld : Date
field open, high, low, close, adjClose: Double
field volume  : Int

-- parse one row of the CSV file into a history record
readHistoryTup (d :: o :: hi :: l :: c :: v :: a :: []) = 
  (pDate   datefld d) <++> 
  (pDouble open o)    <++> 
  (pDouble high hi)   <++> 
  (pDouble low l)     <++> 
  (pDouble close c)   <++> 
  (pInt    volume v)  <++> 
  (pDouble adjClose a)
readHistoryTup _ = Nothing

-- parse an entire CSV of history records
readHistoryFromFile = readDataFile readHistoryTup
readHistoryFromURL  = readDataURL  readHistoryTup

-- show the history table, given the history relation
historyTable = tabular Nothing

-- show a line chart from the history relation
historyChart r = chart
  Nothing Vertical Nothing unit_Fmt unscaledDateRange Nothing unit_Fmt defaultScaled 
  --Nothing Vertical Nothing unit_Fmt defaultScaled Nothing unit_Fmt defaultScaled 
  [
  --line (prim "Open")      datefld open     r
  --,line (prim "High")      datefld high     r
  --,line (prim "Low")       datefld low      r
  --,line (prim "Close")     datefld close    r
  line (prim_Op "Adj Close") (dateRange_Op datefld datefld) adjClose r
  --line (prim_Op "Adj Close") datefld adjClose r
  ]

-- ======= Examples ======= --

--aapl = readHistoryFile "aapl.csv"
aapl = yahoo "AAPL" @2012/10/22 @2012/11/22
goAaplChart = go historyChart aapl
goAaplString = yahooString "AAPL" @2012/10/22 @2012/11/22

hpq  = yahoo "HPQ"  @2012/10/22 @2012/11/22
goHpqChart = go historyChart hpq


