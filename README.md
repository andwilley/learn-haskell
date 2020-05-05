# Haskell stdin/stdout scripts

## Learn haskell with coding challenges

Created to learn the basics of Haskell

## Setup

### Main program
1. Copy the `starter` directory and rename it whatever you want.
2. Write a program.
3. Compile it with `ghc Main.hs`
4. Run it with `input.txt | ./Main`.
4. a. Run it with `input.txt | ./Main >> output.txt` to write to the output.

### Unit Tests
1. Compile the test with `ghc -main-is Test Test.hs`
2. Run the test with `./Test`

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
