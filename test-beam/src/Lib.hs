module Lib
    ( someFunc, isLeapYear
    ) where

isLeapYear :: Integer -> Bool
isLeapYear n = divides 4 && not (divides 100) || divides 400
  where divides x = mod n x == 0

someFunc :: IO ()
someFunc = print (isLeapYear 300)
