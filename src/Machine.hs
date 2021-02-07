{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
-- ScopedTypeVariables to print function types signature at runtime
-- DeriveGeneric to derive ToJSON and FromJSON

module Machine where

 -- Show functions

-- Generic for ToJSON/FromJSON
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

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
    jName :: String,
    jAlphabet :: [[Char]],
    jBlank :: Char,
    jInitial :: String,
    jFinals :: [String],
    jTransitions :: JTransitions 
} deriving (Generic, Show)

-- The LANGUAGE pragma and Generic instance let us write empty FromJSON and ToJSON instances for which the compiler will generate sensible default implementations.

-- No need to provide a parseJSON implementation.

-- No need to provide a toJSON implementation.
-- For efficiency, we write a simple toEncoding implementation, as
-- the default version uses toJSON.

instance ToJSON JMachine where
instance FromJSON JMachine

instance ToJSON JTransitions where
instance FromJSON JTransitions

instance ToJSON JTransition where
instance FromJSON JTransition


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

genericTransition :: current Char -> to_write Char -> Move -> to_state String -> transitionsList [Transition] -> currState State -> newState Either String State  
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
        mName = jName jm,
        mAlphabet = foldl (\acc curr_elem -> (head curr_elem):acc) [] (jAlphabet jm), -- foldl : func acc target -- TO CHECK: list has only one elem
        mBlank = jBlank jm,
        mFinals = jFinals jm,
        mTransitions = Map.empty
    },
    State {
        tape = tape,
        pos = 0,
        nextTransition = jInitial jm
    })
