import Data.Char (toLower)

main :: IO ()
main = do
    contents <- getContents
    let inputLines = lines contents
        results = processPairs inputLines
    mapM_ print results

processPairs :: [String] -> [Int]
processPairs [] = []
processPairs (v:frase:resto) =
    let vogais = map toLower v
        fraseMin = map toLower frase
        total = length [c | c <- fraseMin, c `elem` vogais]
    in total : processPairs resto
processPairs _ = [] 
