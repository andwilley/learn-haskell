# Haskell stdin/stdout scripts

## Learn haskell with coding challenges

Created to learn the basics of Haskell

## Setup

### Main program
1. issue `stack new project-name`
1. `mkdir static && touch static/input.txt`
1. edit app/Main.hs
1. `stack build`
1. `stack exec project-name-exe -- static/input.txt`

Note: consider adding deps to `package.yaml`

### Unit Tests
1. `stack build`
2. `stack test`

### Run GHCID
1. From script directory
2. Execute `ghcid`
Note: `chmod go-w <script-dir>` will fix cryptic "won't start" errors.

## What this gets you

* `Main.hs` A starter script that reads from stdin and writes to stout.
* `input.txt` A file to put your test input.
* `output.txt` An output file if you need it.
* `.ghci` A file so you can run ghcid from the script directory.
* `Test.hs` A template for unit tests.
