module Main where

import qualified Lib

main :: IO ()
main = do
    putStrLn "websocket server"
    Lib.main
