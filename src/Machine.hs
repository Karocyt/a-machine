{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- ScopedTypeVariables to print function types signature at runtime
-- OverloadedStrings allows for seemless conversion between Text/String (not ByteString ?!) as needed

module Machine where

-- For FromJSON
import Data.Aeson.Types (Parser, Object, Value)
import qualified Data.HashMap.Strict as HM
import Data.Aeson (FromJSON(..), parseJSON, (.:), withObject)
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (join)

-- for Map (String, Char) Transition
import Data.Map (Map)
import qualified Data.Map as Map -- functions names clash with Prelude, not Map type itself

-- Show Functions
import Data.Typeable
instance (Typeable a, Typeable b) => Show (a->b) where
  show _ = show $ typeOf (undefined :: a -> b)

type Tape = String -- might be better with a Array-like Map or similar ? Thinking about infinite Tape
type Move = Int
-- Map added to parameters for now as I can't see how to properly curry it
-- type Transition = Map (String, Char) Transition -> State -> Either String State
newtype Transition = Transition { runTransition :: Map (String, Char) Transition -> State -> Either String State } deriving Show

-- Static Machine type
data Machine = Machine {
    name :: String,
    alphabet :: Set Char,
    blank :: Char,
    finals :: [String],
    transitions :: Map (String, Char) Transition,
    initial :: String
} deriving (Show)

-- State type
data State = State {
    tape :: Tape,
    pos :: Int,
    nextTransition :: String -- should be Transition
} deriving (Show)

-- Intermediate type, used while building the Machine
data TransitionStruct = TransitionStruct {
    tName :: String,
    tRead :: Char,
    tToState :: [Char],
    tWrite :: Char,
    tMove :: Move
} deriving (Show)

stringToMove :: String -> Either String Move
stringToMove "LEFT" = Right (-1)
stringToMove "RIGHT" = Right 1
stringToMove s = Left ("Invalid direction in a Transition: '" ++ s ++ "'")

-- toWrite Char -> Move -> toState String -> transitionList Map (String, Char) Transition -> currState State -> newState Either String State 
genericTransition :: Char -> Move -> String -> Map (String, Char) Transition -> State -> Either String State  
genericTransition = error "Not implemented yet"
-- TO DO

buildTransition :: String -> Object -> Set Char -> Parser TransitionStruct
buildTransition name fields mAlphabet = do -- Parser
        tmpRead <- fields .: "read"
        if Set.member tmpRead mAlphabet
            then pure True else fail $ "Function '"++ name ++ "' is able to read '" ++ tmpRead:"' which is not in the machine alphabet."
        tmpToState <- fields .: "to_state"
        tmpWrite <- fields .: "write"
        if Set.member tmpWrite mAlphabet
            then pure True else fail $ "On '" ++ tmpRead:"', function '"++ name ++ "' is able to write '" ++ tmpWrite:"' which is not in the machine alphabet."
        tmpAction <- fields .: "action"
        tmpMove <- case (stringToMove tmpAction) of
            Left s -> fail s
            Right m -> return m
        return $ TransitionStruct name tmpRead tmpToState tmpWrite tmpMove

parseTransitions :: Value -> Set Char -> Parser [Parser TransitionStruct]
parseTransitions raw mAlphabet =
    -- Conversion function + composition operator
    foldl (\globalAcc (name, linesArray :: [Object]) ->
        (foldl (\nameAcc lineObject ->
            (buildTransition name lineObject mAlphabet):nameAcc) [] linesArray) ++ globalAcc) []
        .
    -- Turn the HashMap with random name into a list of pairs (name, [objects]) and apply (<$>) operator
    HM.toList <$>
    -- parse the JSON thing into a HashMap String (HashMap String a)
    parseJSON raw

onlyUnique :: Eq a => [a] -> Bool
onlyUnique [] = True
onlyUnique (x:xs) = if elem x xs then False else onlyUnique xs 

-- Following https://artyom.me/aeson tutorial
instance FromJSON Machine where
    parseJSON = withObject "machine" $ \o -> do -- in Parser (kinda Either String Value)
        mName <- o .: "name"
        alphabetStrings <- o .: "alphabet" :: Parser [String]
        let lAlphabet = foldl (\acc curr_elem -> (head curr_elem):acc) [] alphabetStrings
        if (onlyUnique lAlphabet)
            then pure True else fail "Duplicates found in alphabet"
        mBlank <- o .: "blank"
        if (elem mBlank lAlphabet)
            then pure True else fail "Blank char missing in alphabet"
        let mAlphabet = Set.fromList lAlphabet
        mFinals <- o .: "finals"
        mInitial <- o .: "initial" :: Parser String
        transitionsListObject <- o .: "transitions" -- > Parser Object
        transitionsListParsed <- parseTransitions transitionsListObject mAlphabet -- [Parser TransitionStruct] <- (Parser Object -> (Parser [Parser TransitionStruct])) 
        transitions <- sequence $ transitionsListParsed -- [t] <- [Parser t] -> Parser [t]
        -- can't include the Map in curryied genericTransition... ?!
        let mTransitions = foldl (\acc currT -> Map.insert (tName currT, tRead currT) (Transition (genericTransition (tWrite currT) (tMove currT) (tToState currT))) acc) Map.empty transitions
        return Machine{name=mName, alphabet=mAlphabet, blank=mBlank, finals=mFinals, transitions=mTransitions, initial=mInitial}

-- NEEDS:
-- - check if nextTransition in finals
--   - Right State
-- - Execute/check if transition exists
--   - Left "error blabla"
-- - Move pos
--   - Left "Stay on the dancefloor"
-- - Tail recursion
-- State might get bigger depending on how we handle bonuses (keeping history, counting iterations...)

-- TO DO
runMachine :: Machine -> State -> Either String State
runMachine machine state = Right state -- Left $ "It's DEAD" -- :\n" ++ (show machine) ++ "\n" ++ (show state)
