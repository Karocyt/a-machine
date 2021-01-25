{-# LANGUAGE DeriveGeneric #-}

module Machine where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

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

instance ToJSON JMachine where
instance FromJSON JMachine

instance ToJSON JTransitions where
instance FromJSON JTransitions

instance ToJSON JTransition where
instance FromJSON JTransition


-- State is composed of:
-- - tape
-- - position
-- - currTransition

-- Tape is an alias for String
-- Direction is (+1) or (-1) -- bounds checks ?

-- Transition is of type State -> State
-- Behing the scenes, Transition is a partially applied GenericTransition of type
-- (State -> read Char -> write Char -> Move -> to_state String) -> newState State  

-- Machine should have:
-- - name
-- - alphabet
-- - blank
-- - finals
-- - transitions: map of String -> Transition

-- Runner is a tail call stopping when currTransition is in finals

-- all funcs return Either String a
-- all func are applied with fmap and consorts, passing errors all along
-- main check the Either for an error String or print the final tape 

-- FLOW:
-- BasicCheckArgs => buildMachine => runMachine
-- BuildMachine will need heavy constructors
-- BasicCheck args could disappear with even heavier Machine smart constructors
-- is managing missing tape with Either too much ?
