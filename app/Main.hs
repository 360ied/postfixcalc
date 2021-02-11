module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)

handleOperator :: Fractional a => String -> [a] -> [a]
handleOperator op (a : b : s) =
  case op of
    "+" -> b + a : s
    "-" -> b - a : s
    "*" -> b * a : s
    "/" -> b / a : s

exec :: [String] -> [Double] -> [Double]
exec [] stack = stack
exec (token : tokens) stack =
  case readMaybe token :: Maybe Double of
    Just n -> exec tokens (n : stack)
    Nothing -> exec tokens $ handleOperator token stack

main :: IO ()
main = do
  args <- getArgs
  let stack = exec args []
  if length stack == 1
    then print $ head stack
    else print stack
