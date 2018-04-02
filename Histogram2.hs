import Data.List.Split 
import Data.List 
import Data.Char 

toWordList :: String -> [String]
toWordList file = splitOn ";" (removeRepeatesdSimicolon0
                                     [ if not(isLetter char) 
                                         then ';'
                                         else (toLower char)
                                       | char <- file 
                                     ]
                               )

removeRepeatesdSimicolon0 :: String -> String
removeRepeatesdSimicolon0 (';':xs)=removeRepeatesdSimicolon xs
removeRepeatesdSimicolon0 xs = removeRepeatesdSimicolon xs
removeRepeatesdSimicolon :: String -> String
removeRepeatesdSimicolon [] = []
removeRepeatesdSimicolon (';':[]) = []
removeRepeatesdSimicolon (';':';':xs) = ';':removeRepeatesdSimicolon xs
removeRepeatesdSimicolon (x:xs) = x:removeRepeatesdSimicolon xs
commonWords :: [String]
commonWords =  ["the","be","to","of","and","a","in","that","have","i",
               "it","for","not","on","with","he","as","you","do","at"]

countCommonWords :: [String] -> Int
countCommonWords xs = myCount (filter  (`elem` commonWords) xs)
                      where
                       myCount [] = 0
                       myCount (_:xs) = 1 + myCount xs

dropCommonWords :: [String] -> [String]
dropCommonWords xs = reverse (sort [x | x<-xs ,not (elem x commonWords)])
countWords :: [String] -> [(String,Int)]
countWords [] = []
countWords (x:xs) = (x,wordCount) : countWords (filter (/=x) xs)
                           where
                            wordCount = 1 + myCount (filter (== x) xs)
                            myCount [] = 0
                            myCount (_:xs) = 1 + myCount xs

sortWords :: [(String,Int)] -> [(String,Int)]
sortWords [] = []
sortWords xs = let maxFreq = maximum (map snd xs) in (zip ([fst x | x <- xs , maxFreq == snd x]) [maxFreq,maxFreq..])
                                                     ++ sortWords (filter (\x -> maxFreq /= snd x) xs)

makeHistogram :: [(String,Int)] -> String
makeHistogram list = histogramHelper list 1 
                       where
                        histogramHelper :: [(String,Int)] -> Int -> String
                        histogramHelper [] _ = ""
                        histogramHelper _ 21 = ""
                        histogramHelper ((x,y):xs) count = (replicate y '*') ++ " -> " ++ x ++ "\n" ++ (histogramHelper xs (count+1))
main = do
  putStrLn "Introduce a filename:"
  fname <- getLine
  textdata <- readFile fname
  let wordlist = toWordList textdata
  putStrLn "Report:"
  putStrLn ("\t" ++ (show $ length wordlist) ++ " words")
  putStrLn ("\t" ++ (show $ countCommonWords wordlist) ++ " common words")
  putStrLn "\nHistogram of the most frequent words (excluding common words):"
  putStr $ makeHistogram $ sortWords $ countWords $ dropCommonWords $ wordlist
