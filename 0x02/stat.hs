import Data.Char 
import qualified Data.Map.Lazy as Map
import Data.List
import Control.Exception -- try
import Text.Printf

main = do
    -- OPEN FILE
    file <- readFile "/tmp/test.txt"

    -- GENEREATE STRUCTURES
    let d_words  = Map.fromListWith (+) [(word,1) | word<-words (map toLower file)]
    let d_lengths = Map.fromListWith (+) [(length word, num) | (word, num) <- Map.assocs d_words]

    -- GENEREATE STATISTICS
    let tot = Map.foldl (+) 0 d_lengths
    --let avg =  (Map.foldlWithKey (\key acc val -> acc+val*key) 0 d_lengths) `quot`  tot
    let avg =  (Map.foldlWithKey (\key acc val -> acc+val*key) 0 d_lengths) `quot`  tot -- FIX
    let stat = Map.assocs d_lengths
    let _popular = sortBy (\a b-> (flip compare) (snd a) (snd b)) (Map.assocs d_words)
    let popular = take 10 _popular

    -- OUTPUT
    putStrLn $ printf "Total Words: %d\n" tot
    putStrLn $ printf "Average Length: %d\n" avg
    putStrLn $ "Top Lengths:\n" ++ unlines (map (\(a,b) -> printf "%3d chars => %5d" a b) stat)
    putStrLn $ "Top Lengths:\n" ++ unlines (map (\(i,(a,b)) -> printf "%2d %10s => %5d" i a b) (zip ([1..]::[Int]) popular))

