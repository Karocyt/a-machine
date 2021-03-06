{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for lambdas when type is unclear
{-# LANGUAGE OverloadedStrings #-} -- Seemless Text/String (not ByteString ?!) conversion for (.:) param

module Machine where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map -- functions names clash with Prelude, not Map type itself
import Data.List (intercalate, groupBy)
import Data.Either (isLeft, fromLeft)
import Control.Monad (join)
import Data.HashMap.Strict (toList)
import Data.Aeson (FromJSON, parseJSON, (.:), withObject)
import Data.Aeson.Types (Parser, Object, Value)

instance Show Transition where
  show _ = "t"

type Tape = Seq Char
type Move = Int
newtype Transition = Transition { runTransition :: Map (String, Char) Transition -> State -> State }

-- Static Machine type
data Machine = Machine {
    name :: String,
    alphabet :: Set Char,
    blank :: Char,
    finals :: [String],
    transitions :: Map (String, Char) Transition,
    initial :: String
} --deriving (Show)

instance Show Machine where
    show (Machine name alphabet blank finals transitions initial) = (concat ["*" | _ <- [1..42]]) ++
        "\n\t" ++ name ++
        '\n':(concat ["*" | _ <- [1..42]]) ++
        "\nalphabet: " ++ intercalate ", " (((map show).Set.toList) alphabet) ++
        "\nblank: '" ++ blank:"'" ++
        "\ninitial: " ++ initial ++
        "\nfinals: " ++ (intercalate ", " finals) ++
        "\ntransitions:\n\t- " ++ (((intercalate "\n\t- ").map (\(group) -> (show.fst.fst.head) group ++ " defined for:\n\t\t" ++ intercalate ", " (map (show.snd.fst) group))) $ groupBy (\a b -> (fst.fst) a == (fst.fst) b) $ Map.toList transitions) ++
        '\n':(concat ["*" | _ <- [1..42]])

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

stateFromString :: String -> Int -> Machine -> State
stateFromString tapeStr pos m = State {
    tape=Seq.fromList tapeStr,
    pos=pos,
    nextTransition=initial m
}

currChar :: State -> Either String Char
currChar state = case (Seq.lookup (pos state) (tape state)) of
    Nothing -> Left $ "Error reading tape: Out of bounds while reading pos " ++ (show (pos state))
    Just x -> Right x

stringToMove :: String -> Either String Move
stringToMove "LEFT" = Right (-1)
stringToMove "RIGHT" = Right 1
stringToMove s = Left ("Invalid direction in a Transition: '" ++ s ++ "'")

genericTransition :: Char -> Move -> String -> Map (String, Char) Transition -> State -> State  
genericTransition toWrite move toTransition transitions state = do
    let newTape = Seq.update (pos state) toWrite (tape state)
    let newPos = (pos state) + move
    let nextT = toTransition
    State {tape=newTape, pos=newPos, nextTransition=nextT}

buildTransition :: String -> Object -> Set Char -> Parser TransitionStruct
buildTransition name fields mAlphabet = do
        tmpRead <- fields .: "read"
        if Set.member tmpRead mAlphabet
            then pure True else fail $ "Function '"++ name ++ "' is able to read '" ++ tmpRead:"' which is not in the machine alphabet."
        tmpToState <- fields .: "to_state"
        tmpWrite <- fields .: "write"
        if Set.member tmpWrite mAlphabet
            then pure True else fail $ "On '" ++ tmpRead:"', function '"++ name ++ "' is able to write " ++ (show tmpWrite) ++ " which is not in the machine alphabet."
        tmpAction <- fields .: "action"
        tmpMove <- case (stringToMove tmpAction) of
            Left s -> fail s
            Right m -> return m
        return $ TransitionStruct name tmpRead tmpToState tmpWrite tmpMove

parseTransitions :: Value -> Set Char -> Parser [Parser TransitionStruct]
parseTransitions raw mAlphabet =
    foldl (\globalAcc (name, linesArray :: [Object]) ->
        (foldl (\nameAcc lineObject ->
            (buildTransition name lineObject mAlphabet):nameAcc) [] linesArray) ++ globalAcc) []
        . -- Conversion function + composition operator
    toList <$> -- Turn the HashMap with random name into a list of pairs (name, [objects]) and apply (<$>) operator
    parseJSON raw -- parse the JSON thing into a HashMap String (HashMap String a)

onlyUnique :: Eq a => [a] -> Bool
onlyUnique [] = True
onlyUnique (x:xs) = if elem x xs then False else onlyUnique xs

instance FromJSON Machine where
    parseJSON = withObject "machine" $ \o -> do -- in Parser (kinda Either String Value)
        mName <- o .: "name"
        alphabetStrings <- o .: "alphabet" :: Parser [String]
        if elem True (foldl (\acc (x:xs) -> if xs == []
            then False:acc
            else True:acc
            ) [] alphabetStrings)
        then fail "Alphabet values cannot contain more than 1 character"
        else pure True
        let lAlphabet = foldl (\acc curr_elem -> (head curr_elem):acc) [] alphabetStrings
        if (onlyUnique lAlphabet)
            then pure True else fail "Duplicates found in alphabet"
        mBlank <- o .: "blank"
        if (elem mBlank lAlphabet)
            then pure True else fail "Blank char missing in alphabet"
        let mAlphabet = Set.fromList lAlphabet
        mFinals <- o .: "finals"
        mInitial <- o .: "initial" :: Parser String
        -- Cleaning could be made
        transitionsListObject <- o .: "transitions" -- > Parser Object
        transitionsListParsed <- parseTransitions transitionsListObject mAlphabet -- [Parser TransitionStruct] <- (Parser Object -> (Parser [Parser TransitionStruct])) 
        transitions <- sequence $ transitionsListParsed -- [t] <- [Parser t] -> Parser [t]
        -- 
        let keyTuples = foldl (\acc t -> (tName t, tRead t):acc) [] transitions
        if onlyUnique keyTuples
            then pure True else fail "Duplicate definitions in transitions"
        let mTransitions = foldl (\acc t -> Map.insert (tName t, tRead t) (Transition (genericTransition (tWrite t) (tMove t) (tToState t))) acc) Map.empty transitions
        return Machine{name=mName, alphabet=mAlphabet, blank=mBlank, finals=mFinals, transitions=mTransitions, initial=mInitial}

runMachine :: Machine -> State -> Either String State
runMachine machine state    | elem (nextTransition state) (finals machine) = Right state
                            | isLeft (currChar state) = Left $ fromLeft "" $ currChar state
                            | otherwise = do
                                c <- currChar state
                                let t = nextTransition state
                                let maybeT = Map.lookup (t, c) (transitions machine)
                                nextT <- case (maybeT) of
                                    Nothing -> Left $ "Behavior is not defined for state '" ++ t ++ "' and symbol " ++ (show c)
                                    Just x -> Right x
                                let newState = (runTransition nextT) (transitions machine) state
                                runMachine machine newState
