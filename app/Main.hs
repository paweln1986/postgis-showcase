module Main where
import Api.Http (serveApi)

main :: IO ()
main = do 
    putStrLn "starting server on port 8000!"
    serveApi
