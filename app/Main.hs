module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import System.Exit (exitFailure)
import Control.Exception (try, SomeException)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson (eitherDecode)

import Machine

usage :: String
usage = "Usage: ./a-machine desc.json tape\nwhere:\n\t- 'desc.json' is a json encoded file containing a valid machine description\n\t- 'tape' is a string of instructions from the machine alphabet"

processArgs :: [String] -> Either String [String]
processArgs xs  | length xs /= 2    = Left usage
                | otherwise         = Right xs

eitherRead :: String -> IO (Either String String)
eitherRead filename = do
    res <- try (readFile filename) :: IO (Either SomeException String)
    case res of
        Left ex         -> pure $ Left ("Unable to read " ++ filename ++ " without black magic:\n\t" ++ show ex ++ "\n" ++ usage)
        Right content   -> pure $ Right content

buildMachine :: Either String [String] -> IO (Either String Machine)
buildMachine (Left str)     = pure $ Left str
buildMachine (Right args)   = do
    let filename = head args
    content <- eitherRead filename
    pure $ content >>= eitherDecode.pack

buildInitialState :: Either String Machine -> Either String [String] -> Either String State
buildInitialState (Left err1) _ = Left err1
buildInitialState _ (Left err2) = Left err2
buildInitialState (Right machine) (Right args) = (buildState (args !! 1) $ initial machine)

debug :: IO ()
debug = putStrLn "----- END (All exceptions/errors handled properly) -----"

main :: IO ()
main = do
    putStrLn "----- BEGIN --------------------------------------------"
    -- IO stuff stays in main then byebye IO
    args        <- processArgs <$> getArgs
    machine     <- buildMachine args

    -- realMain returns the last tape
    case ( runMachine <$> machine <*> buildInitialState machine args ) of -- POWPOWPOW
        Left str    -> putStrLn str >> putStrLn "----- END (Exceptions or Errors handled properly) ------" >> exitFailure
        Right state -> putStrLn ("It's ALIIIIIVE:\n" ++ (show machine) ++ "\n" ++ (show state)) >> putStrLn "----- END (No errors) ----------------------------------"
