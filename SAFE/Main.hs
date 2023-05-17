module Main where

import PGF
import Data.List
import System.IO (hFlush, stdout)
import Disambiguate (disambiguate)


main :: IO ()
main = do
  gr <- readPGF "SAFEQuery.pgf"
  putStrLn "Write your sentence here. If it's ambiguous, I will ask for clarification."
  putStrLn "Write quit to exit."
  loop (apply disambiguate gr)

loop :: (String -> String) -> IO ()
loop trans = do
  putStr "> "
  hFlush stdout
  s <- getLine
  if s == "quit" then putStrLn "bye" else do
    putStrLn $ trans s
    loop trans

apply :: (Tree -> [Tree]) -> PGF -> String -> String
apply tr gr s = case parseAllLang gr (startCat gr) s of
  (lg,[t]):_ -> unlines [
    "The input is not ambiguous.",
    showExpr [] t]

  (lg,ts@(x:xs)):_ -> unlines $
    "":
    "The sentence has the following interpretations:":
    "":
    nub [ "* " <> unlines
          (prTreeAndLin gr lg <$> interpretations)
        | t <- ts
        , let interpretations = tr t ]

  _ -> case last s of
    '.' -> noparse
    _   -> let variants = [
                 result
                 | suf <- [" .", " , ."]
                 , let result = apply tr gr (s ++ suf)
                 , result /= noparse ]
           in last $ noparse:variants

  where
    noparse = "Can't parse the input."
    prTreeAndLin gr lg tree = unlines [linearize gr lg tree, showExpr [] tree]
