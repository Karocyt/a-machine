# a-machine

## Requirements

### Windows
Download and install [Stack](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

### Unix systems (including MacOs X)
```bash
curl -sSL https://get.haskellstack.org/ | sh
```
or
```bash
wget -qO- https://get.haskellstack.org/ | sh
```

### MacOs X Homebrew
```bash
brew install haskell-stack
```

## Build

```bash
stack build
```

## Run

```bash
stack exec a-machine-exe desc.json tape
```
where:
- `desc.json` is a json encoded file containing a valid machine description
- `tape` is a string of instructions from the machine alphabet

## Delete

```bash
rm -r .stack-work
```

# Notepad:

The json fields are defined as follows:
 - name: The name of the described machine

 - alphabet: Both input and work alphabet of the machine merged into a single alphabet for simplicity’s sake, including the blank character. Each
     character of the alphabet must be a string of length strictly equal to 1.

 - blank: The blank character, must be part of the alphabet, must NOT be
     part of the input.

 - states: The exhaustive list of the machine’s states names.

 - initial: The initial state of the machine, must be part of the states list.

 - finals: The exhaustive list of the machine’s final states. This list must be a
     sub-list of the states list.

 - transitions: A dictionnary of the machine’s transitions indexed by state
     name. Each transition is a list of dictionnaries, and each dictionnary
     describes the transition for a given character under the head of the
     machine. A transition is defined as follows:

     - read: The character of the machine’s alphabet on the tape under the
        machine’s head.

     - to_state: The new state of the machine after the transition is done.

     - write: The character of the machine’s alphabet to write on the tape
        before moving the head.

     - action: Movement of the head for this transition, either LEFT, or
        RIGHT.


State is composed of:
- tape
- position
- nextTransition

Tape is an alias for String
Move is (+1) or (-1) -- bounds checks in type ?

Transition is of type State -> Either String State
Behing the scenes, Transition is a partially applied GenericTransition of type
(read Char -> write Char -> Move -> to_state String -> currState State -> transitionsList [Transition]) -> newState State
we'll need to first generate partially applied funcs without transitionsList then apply transitionList once all transitions are loaded

Machine should have:
- name: String
- alphabet: [Char]
- blank: Char
- finals: [String]
- transitions: Map (String Char) Transition

Runner is a tail call stopping when currTransition is in finals

all funcs return Either String a
all func are applied with fmap and consorts, passing errors all along
main check the Either for an error String or print the final tape 

FLOW:
BasicCheckArgs => buildMachine => runMachine
might need an intermediary layer to avoid the IO context in BuildMachine
BuildMachine will need heavy constructors
BasicCheck args could disappear with even heavier Machine smart constructors
is managing missing tape with Either too much ?
`json` package might sound more "standard library" than `aeson`

State might get bigger depending on how we handle bonuses (keeping history, counting iterations...)
