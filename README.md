# Checkers-Ai

## Usage

This project implements the minimax along with alpha beta pruning for the game of checkers.

There are two implementation, one, in common lisp was done first, as a way to translate psoeudo recursive code in an easy way. The second implementation, done in c, is much more focused on performance, as a way to have better search depth and to construct the n move database.

Here is a short list of ai related tasks that this project implements:

- game of checkers
- minimax algorithm
- alpha beta pruning for minimax algorithm
- iterative deepening for minimax algorithm
- checkers move ordering
- frequency hashtable for move ordering
- killer move array for move ordering
- transposition table for move ordering
- genetic algorithm for training utility function
- opening database
- 4 piece end game database

## Installation

In the directory release are two sub directories containing binaries for both programs.
Therefore, to run the program, you should always just be able to do :

```bash
# for the c program
cd release/c
./checkers-ai

# for the lisp program
cd release/lisp
./checkers-ai
```

Here is a guide on how to compile and run the programs.

### For the c program

### For the lisp program
