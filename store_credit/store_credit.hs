import Data.List.Split
import Data.List
import Data.Maybe

main = do 
    l <- readFile ("A-small-practice.in")
    let _lines = lines l
        count = head _lines
        _cases = chunksOf 3 $ tail _lines
        cases = map (\x -> splitOn " " x) (map last _cases)
        pass = zip (map init _cases) cases

    print $ map (\x -> matchCredit x) pass
    --print $ map last _cases

    --print $ (read count) == (length cases)

-- assuming given [C, I, [P]]
matchCredit (cInfo, prices) = map (\x -> 1 + (fromJust $ elemIndex x items)) _prices
    where
        cred = read (head cInfo) :: Int
        items = map (\x -> read x :: Int) prices
        _prices = [ x | x <- items, y <- (items \\ [x]), x + y == cred ]
