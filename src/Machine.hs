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

data JTransition = JTransition {
    read :: Char,
    to_state :: [Char],
    write :: Char,
    action :: String
} deriving (Generic, Show)

data JTransitions = JTransitions {
    scanright :: [JTransition],
    eraseone :: [JTransition],
    subone :: [JTransition]
} deriving (Generic, Show)

data JMachine = JMachine {
    name :: String,
    alphabet :: [[Char]],
    blank :: Char,
    initial :: String,
    finals :: [String],
    transitions :: JTransitions 
} deriving (Generic, Show)

-- The LANGUAGE pragma and Generic instance let us write empty FromJSON and ToJSON instances for which the compiler will generate sensible default implementations.

-- No need to provide a parseJSON implementation.

-- No need to provide a toJSON implementation.
-- For efficiency, we write a simple toEncoding implementation, as
-- the default version uses toJSON.

instance ToJSON JMachine

instance ToJSON JTransitions

instance ToJSON JTransition


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
type Transition = State -> Either String State

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

-- toWrite Char -> Move -> toState String -> currState State -> newState Either String State 
genericTransition :: Char -> Move -> String -> State -> Either String State  
genericTransition = error "Not implemented yet"

data Machine = Machine {
    mName :: String,
    mAlphabet :: [Char],
    mBlank :: Char,
    mFinals :: [String],
    mTransitions :: Map (String, Char) Transition
} deriving (Show)

buildMachine :: JMachine -> Tape -> Either String (Machine, State)
buildMachine jm tape = Right (
    Machine {
        mName = name jm,
        mAlphabet = foldl (\acc currElem -> (head currElem):acc) [] (alphabet jm), -- foldl : func acc target -- TO CHECK: list has only one elem
        mBlank = blank jm,
        mFinals = finals jm,
        mTransitions = Map.empty
    },
    State {
        tape = tape,
        pos = 0,
        nextTransition = initial jm
    })

data TransitionObject = TransitionObject {
    tName :: String,
    tRead :: Char,
    tToState :: [Char],
    tWrite :: Char,
    tMove :: Move
} deriving (Generic, Show)

-- Following https://artyom.me/aeson tutorial

-- parseTransitions :: Value -> Parser [TransitionObject]
parseTransitions raw =
    -- Conversion function + composition operator
    map (\(name, fields) -> do -- Parser
        tmpRead <- fields .: "read"
        tmpToState <- fields .: "to_state"
        tmpWrite <- fields .: "write"
        tmpAction <- fields .: "action"
        tmpMove <- case (stringToMove tmpAction) of
            Left s -> error s
            Right m -> return m
        return (TransitionObject name tmpRead tmpToState tmpWrite tmpMove))
        .
    -- Turn the HashMap with random name into a list of pairs (name, fields), and apply (<$>) operator
    HM.toList <$>
    -- parse the JSON thing into a HashMap String (HashMap String a)
    parseJSON raw

instance FromJSON Machine where
    parseJSON = withObject "machine" $ \o -> do
        mName <- o .: "name"
        alphabetStrings <- o .: "alphabet"
        let mAlphabet = foldl (\acc curr_elem -> (head curr_elem):acc) [] alphabetStrings
        mBlank <- o .: "blank"
        mFinals <- o .: "finals"
        transitionsListObject <- o .: "transitions"
        transitionsListParsed <- parseTransitions transitionsListObject
        -- can't include the Map in curryied genericTransition... ?!
        let mTransitions = foldl (\acc currT -> Map.insert (tName currT, tRead currT) (genericTransition (tWrite currT) (tMove currT) (tToState currT)) acc) Map.empty
        return Machine{mName=mName, mAlphabet=mAlphabet, mBlank=mBlank, mFinals=mFinals, mTransitions=mTransitions}