module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Parser
import Machine
import Runner

import Control.Exception (try, SomeException)

-- both args and content might need to be IO (Either String a) ?
-- I broke my teeth on readFile, my best being IO (Either error Bytestring) at some point 

usage :: String
usage = "Usage: ./a-machine desc.json tape\nwhere:\n\t- 'desc.json' is a json encoded file containing a valid machine description\n\t- 'tape' is a string of instructions from the machine alphabet"

eitherRead :: String -> IO (Either String String)
eitherRead filename = do
    res <- try (readFile filename) :: IO (Either SomeException String)
    case res of
        Left ex -> pure $ Left ("Exception while reading " ++ filename ++ ": " ++ show ex ++ "\n" ++ usage)
        Right content -> pure $ Right content

buildMachine :: Either String [String] -> IO (Either String Machine)
buildMachine (Left str) = pure $ Left str
buildMachine (Right args) = do
    let filename = head args
    content <- eitherRead filename
    pure $ content >>= parseMachine

buildState :: String -> String -> Either String State
buildState tape_content initial_state = do
    Right (State {
        tape=tape_content,
        pos=0,
        nextTransition=initial_state
    })

realMain :: Either String Machine -> Either String [String] -> String
realMain (Left err1) _ = err1
realMain _ (Left err2) = err2
realMain (Right machine) (Right args) = do
    let eitherState = buildState (args !! 1) $ mInitial machine
    -- state <- (eitherState :: Either String State)
    case (eitherState) of
        Left str  -> str
        Right last_state   -> tape last_state

-- main :: IO ()
main = do
    putStrLn "-- BEGIN ----------------------------------"
    args <- processArgs <$> getArgs
    putStrLn "- Arguments checked"
    machine <- buildMachine args
    putStrLn "- Machine built"
    -- state <- buildState args $ mInitial machine
    putStrLn $ realMain machine args

    putStrLn "-- END ------------------------------------"

