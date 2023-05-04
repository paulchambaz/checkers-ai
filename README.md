# Checkers-Ai

## Usage

This project implements the minimax along with alpha beta pruning for the game of checkers.

There are two implementation, one, in common lisp was done first, as a way to translate psoeudo recursive code in an easy way. One could rewrite this code in a lower language like c to get better performance.

Here is a short list of ai related tasks that this project implements:

- game of checkers
- minimax algorithm
- alpha beta pruning for minimax algorithm
- iterative deepening for minimax algorithm
- checkers move ordering
- frequency hashtable for move ordering
- killer move array for move ordering
- genetic algorithm for training utility function
- opening database
- 2 piece end game database

## Installation

Here is a guide on how to compile and run the programs.

### For the lisp program

Here are the dependencies required to compile this project:

```
sbcl
quicklisp
```

You also need to have these lines configured in your `.sbclrc`.

```lisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "~/.config/lisp/quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
```

Once everything is installed, you can run in this directory : 

```bash
make build
```

And have the binaries.
