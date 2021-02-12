module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)

handleOperator :: Floating a => [Char] -> [a] -> [a]
handleOperator op [] =
  case op of
    "pi" -> [pi]
handleOperator op [a] =
  case op of
    "exp" -> [exp a]
    "log" -> [log a]
    "sqrt" -> [sqrt a]
    "sin" -> [sin a]
    "cos" -> [cos a]
    "tan" -> [tan a]
    "asin" -> [asin a]
    "acos" -> [acos a]
    "atan" -> [atan a]
    "sinh" -> [sinh a]
    "cosh" -> [cosh a]
    "tanh" -> [tanh a]
    "asinh" -> [asinh a]
    "acosh" -> [acosh a]
    "atanh" -> [atanh a]
    _ -> handleOperator op [] ++ [a]
handleOperator op (a : b : s) =
  case op of
    "+" -> b + a : s
    "-" -> b - a : s
    "*" -> b * a : s
    "/" -> b / a : s
    "^" -> b ** a : s
    _ -> handleOperator op [a] ++ b : s

exec :: [String] -> [Double] -> [Double]
exec [] stack = stack
exec (token : tokens) stack =
  case readMaybe token :: Maybe Double of
    Just n -> exec tokens (n : stack) -- token is a number, push token to stack
    Nothing -> exec tokens $ handleOperator token stack -- token is an operator, execute the operator

main :: IO ()
main = do
  args <- getArgs
  print $ "args: " ++ foldl1 (\a b -> a ++ " " ++ b) args
  let stack = exec args []
  if length stack == 1
    then print $ head stack
    else print stack
