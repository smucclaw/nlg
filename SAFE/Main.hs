module Main where

import PGF
import Data.List
import Disambiguate (disambiguate)


main :: IO ()
main = do
  gr <- readPGF "SAFEQuery.pgf"
  putStrLn "Write your sentence here. If it's ambiguous, I will ask for clarification."
  putStrLn "Write quit to exit."
  loop (f disambiguate gr)

loop :: (String -> String) -> IO ()
loop trans = do
   s <- getLine
   if s == "quit" then putStrLn "bye" else do
     putStrLn $ trans s
     loop trans

f :: (Tree -> [Tree]) -> PGF -> String -> String
f tr gr s = case parseAllLang gr (startCat gr) s of
  (lg,[t]):_ -> unlines [
    "The input is not ambiguous.",
    showExpr [] t]

  (lg,ts@(x:xs)):_ -> unlines $
    "The sentence has the following interpretations":
    "":
    (nub [ "* " ++
      unlines
       (fmap (linearize gr lg) moves)
    | t <- ts , let moves = tr t ])

  _ -> "Can't parse the input."
