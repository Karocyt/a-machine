module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Machine
import Runner

import Control.Exception (try, SomeException)

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 (pack)

usage :: String
usage = "Usage: ./a-machine desc.json tape\nwhere:\n\t- 'desc.json' is a json encoded file containing a valid machine description\n\t- 'tape' is a string of instructions from the machine alphabet"

processArgs :: [String] -> Either String [String]
processArgs xs  | length xs /= 2    = Left usage
                | otherwise         = Right xs

eitherRead :: String -> IO (Either String String)
eitherRead filename = do
    res <- try (readFile filename) :: IO (Either SomeException String)
    case res of
        Left ex -> pure $ Left ("Unable to read " ++ filename ++ " without black magic:\n\t" ++ show ex ++ "\n" ++ usage)
        Right content -> pure $ Right content

buildMachine :: Either String [String] -> IO (Either String Machine)
buildMachine (Left str) = pure $ Left str
buildMachine (Right args) = do
    let filename = head args
    content <- eitherRead filename
    pure $ content >>= eitherDecode.pack

realMain :: Either String Machine -> Either String [String] -> String
realMain (Left err1) _ = err1
realMain _ (Left err2) = err2
realMain (Right machine) (Right args) = do
    let eitherState = buildState (args !! 1) $ initial machine
    let finalState = eitherState >>= (runMachine machine) 

    case (finalState) of
        Left str  -> str
        Right last_state   -> tape last_state

main :: IO ()
main = do
    putStrLn "----- BEGIN --------------------------------------------"
    args <- processArgs <$> getArgs
    -- putStrLn "- Arguments checked"
    machine <- buildMachine args
    -- putStrLn "- Machine built"

    -- realMain returns the last tape or an error message
    putStrLn $ realMain machine args

    putStrLn "----- END (All exceptions/errors handled properly) -----"

