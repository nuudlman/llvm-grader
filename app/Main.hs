module Main (main) where

import Data.Gradescope
import Data.Aeson (encode)


main :: IO ()
main = do
    putStrLn $ show $ encode $ defaultResults { subScore = Just 100 }
