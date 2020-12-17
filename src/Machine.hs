{-# LANGUAGE DeriveGeneric #-}

module Machine where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)


data Transition = Transition {
    read :: Char,
    to_state :: [Char],
    write :: Char,
    action :: String
} deriving (Generic, Show)

data Transitions = Transitions {
    scanright :: [Transition],
    eraseone :: [Transition],
    subone :: [Transition]
} deriving (Generic, Show)

data Machine = Machine {
    name :: String,
    alphabet :: [[Char]],
    blank :: Char,
    initial :: String,
    finals :: [String],
    transitions :: Transitions 
} deriving (Generic, Show)

-- The LANGUAGE pragma and Generic instance let us write empty FromJSON and ToJSON instances for which the compiler will generate sensible default implementations.

-- No need to provide a parseJSON implementation.

-- No need to provide a toJSON implementation.
-- For efficiency, we write a simple toEncoding implementation, as
-- the default version uses toJSON.

instance ToJSON Machine where
instance FromJSON Machine

instance ToJSON Transitions where
instance FromJSON Transitions

instance ToJSON Transition where
instance FromJSON Transition
