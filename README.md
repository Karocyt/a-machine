# a-machine

## Requirements

### Windows
Download and install [Stack](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

### Unix systems (including MacOs)
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
