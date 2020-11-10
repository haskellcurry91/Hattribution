import Text.CSV (parseCSV, Record)
import qualified Data.Set as S
import Data.List.Split
import qualified Data.List
import qualified Data.Map

data Path = Path { chain :: [String]
                 , transfer :: Float
                 , card :: Float
                 } deriving Show

parseItem :: Record -> Path
parseItem record =
  Path { chain    = unqCH $ splitChain chainStr
       , transfer = case reads(transferStr) :: [(Float,String)] of
         [(c, "")] -> c
         _ -> 0

       , card = fromIntegral (cardinality . splitChain $ chainStr) :: Float
       }
  where chainStr    = record !! 0
        transferStr = record !! 1

parseToTuple :: [String] -> Path
parseToTuple record = item
  where item = parseItem record 

main :: IO ()
main = do
  let fileName = "test_shp.csv"
  input <-  readFile fileName
  let csv =  parseCSV fileName input
  either handleError doWork csv

handleError = print

doWork :: [Record] -> IO ()
doWork csv =  print $
              Data.Map.fromListWith (+) $ concat . map calcWeight $
              map parseToTuple $
              filter (not . emptyList) $ tail  csv

splitChain :: String -> [String]
splitChain "" = []
splitChain str = splitOn ">" str


cutCH :: [String] -> [String]
cutCH = take 150 

unqCH :: [String] -> [String]
unqCH = Data.List.nub . cutCH

cardinality :: [String] -> Int
cardinality = length . unqCH 

shapley :: [Path] -> [(String,Float)]
shapley x = concat . map calcWeight $ filterPath x

calcWeight :: Path -> [(String,Float)]
calcWeight elem = map (\x -> (x ,transferVal/cardVal)) $ chain elem
  where transferVal = transfer elem
        cardVal = card elem

filterPath :: [Path] -> [Path]
filterPath x = filter (not . null . chain) $ filter ((/= 0) .transfer) x

testFunc x = Data.Map.fromListWith (+) x

emptyList :: [a] -> Bool
emptyList (chain:transfers:end) = False
emptyList _                     = True
