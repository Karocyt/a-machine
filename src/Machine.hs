{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- ScopedTypeVariables to print function types signature at runtime
-- OverloadedStrings allows for seemless conversion between Text/String (not ByteString ?!) as needed

module Machine where

-- For FromJSON
import Data.Aeson.Types (Parser, Object, Value)
import qualified Data.HashMap.Strict as HM
import Data.Aeson (FromJSON(..), parseJSON, (.:), withObject)

-- for Map (String, Char) Transition
import Data.Map (Map)
import qualified Data.Map as Map -- functions names clash with Prelude, not Map type itself

-- Show Functions
import Data.Typeable
instance (Typeable a, Typeable b) => Show (a->b) where
  show _ = show $ typeOf (undefined :: a -> b)

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
-- TO DO

data Machine = Machine {
    name :: String,
    alphabet :: [Char],
    blank :: Char,
    finals :: [String],
    transitions :: Map (String, Char) Transition,
    initial :: String
} deriving (Show)

data TransitionStruct = TransitionStruct {
    tName :: String,
    tRead :: Char,
    tToState :: [Char],
    tWrite :: Char,
    tMove :: Move
} deriving (Show)

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
    parseJSON = withObject "machine" $ \o -> do -- in Parser (kinda Either String Value)
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
        return Machine{name=mName, alphabet=mAlphabet, blank=mBlank, finals=mFinals, transitions=mTransitions, initial=mInitial}

buildState :: String -> String -> Either String State
buildState tape_content initial_state = do
    Right (State {
        tape=tape_content,
        pos=0,
        nextTransition=initial_state
    })
