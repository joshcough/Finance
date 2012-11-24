module Yahoo where

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
import Relation
import Syntax.Relation
import Layout.Format as Fmt
import Relation.Op as Op
import Layout.Legend as Lg
import Layout.Presentation as P
import Double
import Layout.SortPriority
import Foo

-- ======= Potential library functions ========= --

mmap  = fmap maybeFunctor
lmap  = fmap listFunctor
--iomap = fmap ioFunctor
ioMaybeMap = iomap . mmap 

foreign
  function "com.clarifi.reporting.util.IOUtils" "dump" dump : a -> a

concatStrings : List (String) -> String
concatStrings = foldl (++_String) ""

infixr 5 <++>
--<++> : c <- (a,b) => Maybe {..a} -> Maybe {..b} -> Maybe {..c}
(<++>) a b = liftA2 maybeAp (++) a b
parseField : (String -> Maybe a) -> Field b a -> String -> Maybe {..b}
parseField parseFunction fld = (mmap $ v -> {fld = v}) . parseFunction

-- convert YYYY-MM-DD to MM/DD/YYYY
pDate fld s = parseField parseDate fld $ reform (split_String '-' s) where
  reform (yyyy :: mm :: dd :: []) = concatStrings [mm, "/", dd, "/", yyyy] 
pDouble     = parseField parseDouble
pInt        = parseField (parseInt 10)

readDataFile tupReader = readData tupReader . readCSVFile
readDataURL  tupReader = readData tupReader . readCSVURL

readData tupReader fetchContents = (readDataTups . reverse . drop 1) <$> fetchContents where
  readDataTups ls = mmap relation $ 
    sequence listTraversable maybeMonad (lmap tupReader ls) 

-- todo: define infix operator for this
joinIOM2Rs        = combineIoMaybe (r1 -> r2 -> r1 ** r2)
joinIOM3Rs r      = joinIOM2Rs . (joinIOM2Rs r)
joinIOM4Rs r1 r2  = joinIOM2Rs . (joinIOM3Rs r1 r2)
renameIOMR iomr f = iomr |> ioMaybeMap (rename adjClose f)

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
yahoo startDate endDate sym = readHistoryFromURL $ yahooURL sym startDate endDate
yahoo2011 = yahoo @2011/01/01 @2011/12/31

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
priceTable r = tabular priceLegend $ r # {datefld, adjClose} where
  priceLegend = Just [(datefld, "Date") ^ 0, (adjClose, "Adj Close") ^ 10]_Sorted_Lg

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

-- ========= HW ======== -----

{--
find online broker to paper trade
invest $1M in 4 equities 
access portfolio for 2011
  * annual return
  * avg daily return
  * stddev of daily return
  * sharp ratio
Compare with benchmark: SPY (S&P)
Submit:
  * .pdf printout of your spreadsheet (pdf writer, anyone?)
  * Screenshot of your portfolio online.

table columns:
Date | AAPL (adjClose) | AAPL Cumulative return | AAPL invest | GLD | GLD cum ret | GLD invest | 2 more stocks | 

cum return = todays price / starting price 
invest = total value of my stock holdings for that company
         original investment * cumulative value

then another little table with:
Equities Allocations  ?    | Performance       Fund     Benchmark
Start:    1           1M   | Annual Return     19.58%   ?
AAPL      1.0         1M?  | Avg daily ret              ?
GLD       0.35        350k | Stddev daily ret
other1    0                | Sharp Ratio
other2    0                |

the sum of the allocations column needs to add up to 1.
so, if we put everything in AAPL, the rest must be zero.
find a decent allocation distribution

the performance info on the right is total for all stocks
see what happens to the perf totals if we change the allocations.

calculate the same numbers for the benchmark (SPY)
to see how your portfolio did against the benchmark.

get the historical results for all for stocks every day for the whole year.
put all of this in a table. it seems a little much to have every day,
but whatever, its ok.
--}

-- pull the daily historical data from yahoo for 2011 for 4 stocks
aapl = yahoo2011 "AAPL" 
hpq  = yahoo2011 "HPQ" 
gld  = yahoo2011 "GLD"
spy  = yahoo2011 "SPY"

-- some quick tests to verify sanity --
goAaplString = yahooString "AAPL" @2012/10/22 @2012/11/22
goAaplChart = go historyChart aapl
goAaplTable = go priceTable   aapl

goHpqChart  = go historyChart hpq
goHpqTable  = go priceTable   hpq

goGldChart  = go historyChart gld
goGldTable  = go priceTable   gld

goSpyChart  = go historyChart spy
goSpyTable  = go priceTable   spy

-- ========== put together a table with the closing prices for all the stocks ========= --
field aaplClose, hpqClose, gldClose, spyClose : Double
bigR = joinIOM4Rs (rn aapl aaplClose) (rn hpq hpqClose) (rn gld gldClose) (rn spy spyClose)
 where rn = renameIOMR
bigPriceTable r = tabular bigPriceLegend $ r # {datefld, aaplClose, hpqClose, gldClose, spyClose} 
  where bigPriceLegend  = Just [ (datefld, "Date") ^ 0, 
    (aaplClose, "AAPL") ^ 10, (hpqClose, "HPQ") ^ 10, (gldClose, "GLD") ^ 10, (spyClose, "SPY") ^ 10
  ]_Sorted_Lg
-- run the table in javafx
goBigPriceTable = go bigPriceTable bigR

-- join in a caluclation to a relation. 
-- todo: document this. 
joinCalc initVField initV outputField calc r =
  r ** (relation [{initVField = initV}]) |> [| outputField = calc initValue |]

-- add in the cumulative return for a stock, given its initial value
field initValue, cumRet: Double
joinCumRet initV = joinCalc initValue initV cumRet (x -> adjClose /_Op x) 

-- add in the total value of current holdings, given an initial holding amount.
field holdings, initialInvestment: Double 
-- f is Relation -> Report, r is Relation
joinTotalValue initV = joinCalc initialInvestment initV holdings (x -> cumRet *_Op x)

showAllDataForStock f initialInvestmentValue historicalData =  
  scanRelation historicalData $ ts -> f $ 
    (relation ts) |> 
    (joinCumRet ((head ts) ! open)) |>
    (joinTotalValue initialInvestmentValue)


initialAaplInvest = 350000.0
goAaplTableWithCumRet = go (showAllDataForStock historyTable initialAaplInvest) aapl

