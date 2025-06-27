main :: IO ()
main = do
    contents <- getContents
    let linhas = lines contents
    mapM_ (putStrLn . processar) linhas

processar :: String -> String
processar line =
    let xs = map read (words line) :: [Int]
    in if length xs /= 3 || any (`notElem` [0,1]) xs
       then "*"
       else vencedor xs

vencedor :: [Int] -> String
vencedor [a,b,c]
    | a /= b && a /= c = "A"
    | b /= a && b /= c = "B"
    | c /= a && c /= b = "C"
    | otherwise        = "*"
vencedor _ = "*"







