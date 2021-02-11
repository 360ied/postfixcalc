module Main where

import System.Environment
import Text.Read

handleOperator op (a : b : s) =
  case op of
    "+" -> b + a : s
    "-" -> b - a : s
    "*" -> b * a : s
    "/" -> b / a : s

exec [] stack = stack
exec (token : tokens) stack =
  case readMaybe token :: Maybe Double of
    Just n -> exec tokens (n : stack)
    Nothing -> exec tokens $ handleOperator token stack

main = do
  args <- getArgs
  let stack = exec args []
  print stack
