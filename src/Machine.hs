{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- ScopedTypeVariables to print function types signature at runtime
-- DeriveGeneric to derive ToJSON and FromJSON
-- OverloadedStrings allows for seemless conversion between Text/String/ByteString as needed

module Machine where

-- For ToJSON
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

-- Generic for ToJSON/FromJSON
import GHC.Generics
import Data.Aeson (Value(..), FromJSON(..), parseJSON, (.:), (.=), withObject)

-- for Map (String, Char) Transition
import Data.Map (Map)
import qualified Data.Map as Map -- functions names clash with Prelude, not Map type itself

-- Show Functions
import Data.Typeable
instance (Typeable a, Typeable b) => Show (a->b) where
  show _ = show $ typeOf (undefined :: a -> b)



-- The json fields are defined as follows:
--  - name: The name of the described machine
-- 
--  - alphabet: Both input and work alphabet of the machine merged into a single alphabet for simplicity’s sake, including the blank character. Each
--      character of the alphabet must be a string of length strictly equal to 1.
-- 
--  - blank: The blank character, must be part of the alphabet, must NOT be
--      part of the input.
-- 
--  - states: The exhaustive list of the machine’s states names.
-- 
--  - initial: The initial state of the machine, must be part of the states list.
-- 
--  - finals: The exhaustive list of the machine’s final states. This list must be a
--      sub-list of the states list.
-- 
--  - transitions: A dictionnary of the machine’s transitions indexed by state
--      name. Each transition is a list of dictionnaries, and each dictionnary
--      describes the transition for a given character under the head of the
--      machine. A transition is defined as follows:
-- 
--      - read: The character of the machine’s alphabet on the tape under the
--         machine’s head.
-- 
--      - to_state: The new state of the machine after the transition is done.
-- 
--      - write: The character of the machine’s alphabet to write on the tape
--         before moving the head.
-- 
--      - action: Movement of the head for this transition, either LEFT, or
--         RIGHT.


-- State is composed of:
-- - tape
-- - position
-- - nextTransition

-- Tape is an alias for String
-- Move is (+1) or (-1) -- bounds checks ?

-- Transition is of type State -> Either String State
-- Behing the scenes, Transition is a partially applied GenericTransition of type
-- (read Char -> write Char -> Move -> to_state String -> currState State -> transitionsList [Transition]) -> newState State
-- we'll need to first generate partially applied funcs without transitionsList then apply transitionList once all transitions are loaded

-- Machine should have:
-- - name: String
-- - alphabet: [Char]
-- - blank: Char
-- - finals: [String]
-- - transitions: Map (String Char) Transition

-- Runner is a tail call stopping when currTransition is in finals

-- all funcs return Either String a
-- all func are applied with fmap and consorts, passing errors all along
-- main check the Either for an error String or print the final tape 

-- FLOW:
-- BasicCheckArgs => buildMachine => runMachine
-- might need an intermediary layer to avoid the IO context in BuildMachine
-- BuildMachine will need heavy constructors
-- BasicCheck args could disappear with even heavier Machine smart constructors
-- is managing missing tape with Either too much ?
-- `json` package might sound more "standard library" than `aeson`

-- To return a Either String Machine, all fields might need to be their own types, returning Either in their constructors ?

type Tape = String
-- Map added to parameters for now as I can't see how to properly curry it
-- type Transition = Map (String, Char) Transition -> State -> Either String State
newtype Transition = Transition { runTransition :: Map (String, Char) Transition -> State -> Either String State } deriving Show

type Move = Int

stringToMove :: String -> Either String Move
stringToMove "LEFT" = Right (-1)
stringToMove "RIGHT" = Right 1
stringToMove s = Left ("Invalid direction in a Transition: '" ++ s ++ "'")

data State = State {
    tape :: Tape,
    pos :: Int,
    nextTransition :: String -- should be Transition
} deriving (Show)

-- toWrite Char -> Move -> toState String -> transitionList Map (String, Char) Transition -> currState State -> newState Either String State 
genericTransition :: Char -> Move -> String -> Map (String, Char) Transition -> State -> Either String State  
genericTransition = error "Not implemented yet"

data Machine = Machine {
    mName :: String,
    mAlphabet :: [Char],
    mBlank :: Char,
    mFinals :: [String],
    mTransitions :: Map (String, Char) Transition,
    mInitial :: String
} deriving (Show)

data TransitionStruct = TransitionStruct {
    tName :: String,
    tRead :: Char,
    tToState :: [Char],
    tWrite :: Char,
    tMove :: Move
} deriving (Generic, Show)

-- Following https://artyom.me/aeson tutorial

buildTransition :: String -> Object -> Parser TransitionStruct
buildTransition name fields = do -- Parser
        tmpRead <- fields .: "read"
        tmpToState <- fields .: "to_state"
        tmpWrite <- fields .: "write"
        tmpAction <- fields .: "action"
        tmpMove <- case (stringToMove tmpAction) of
            Left s -> error s
            Right m -> return m
        return $ TransitionStruct name tmpRead tmpToState tmpWrite tmpMove

parseTransitions :: Value -> Parser [Parser TransitionStruct]
parseTransitions raw =
    -- Conversion function + composition operator
    foldl (\globalAcc (name, linesArray :: [Object]) ->
        (foldl (\nameAcc lineObject ->
            (buildTransition name lineObject):nameAcc) [] linesArray) ++ globalAcc) []
        .
    -- Turn the HashMap with random name into a list of pairs (name, [objects]) and apply (<$>) operator
    HM.toList <$>
    -- parse the JSON thing into a HashMap String (HashMap String a)
    parseJSON raw

instance FromJSON Machine where
    parseJSON = withObject "machine" $ \o -> do
        mName <- o .: "name"
        alphabetStrings <- o .: "alphabet" :: Parser [String]
        let mAlphabet = foldl (\acc curr_elem -> (head curr_elem):acc) [] alphabetStrings
        mBlank <- o .: "blank"
        mFinals <- o .: "finals"
        mInitial <- o .: "initial" :: Parser String
        transitionsListObject <- o .: "transitions" -- > Parser Object
        transitionsListParsed <- parseTransitions transitionsListObject -- [Parser TransitionStruct] <- (Parser Object -> (Parser [Parser TransitionStruct])) 
        transitions <- sequence $ transitionsListParsed -- [t] <- [Parser t] -> Parser [t]
        -- can't include the Map in curryied genericTransition... ?!
        let mTransitions = foldl (\acc currT -> Map.insert (tName currT, tRead currT) (Transition (genericTransition (tWrite currT) (tMove currT) (tToState currT))) acc) Map.empty transitions
        return Machine{mName=mName, mAlphabet=mAlphabet, mBlank=mBlank, mFinals=mFinals, mTransitions=mTransitions, mInitial=mInitial}

buildState :: String -> String -> Either String State
buildState tape_content initial_state = do
    Right (State {
        tape=tape_content,
        pos=0,
        nextTransition=initial_state
    })