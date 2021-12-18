# A functional implementation to solve the Traveling Salesman Problem

## Setup
`stack setup`

## Build and Run    
`stack build`    
`stack exec tsp-functional-exe [-s|-p|-c :nN|-g :sN :rN] <filename>`    

or

`stack build --profile`    
`stack exec tsp-functional-exe --profile -- [-s|-p|-c :nN|-g :sN :rN] <filename> +RTS -N4 -s -p -ls`    
(-s: spark info, -p: profiling, -ls: eventlog)    
(N = any integer)    

## Install
`stack install`


## Sample input.txt file (contains the coordinates of the cities)
```
0 0
0 1
1 0
1 1
0 2
2 0
2 1
3 0
3 1
4 0
4 1
```

## Sample running commands
`stack setup`    
`stack build`    
#### Sequential
`stack exec tsp-functional-exe -- -s /Users/jcs/projects/tsp-functional/input.txt +RTS -s`
#### Parallel
`stack exec tsp-functional-exe -- -p /Users/jcs/projects/tsp-functional/input.txt +RTS -s`
#### Parallel with Chunk size
`stack exec tsp-functional-exe -- -c :n1024 /Users/jcs/projects/tsp-functional/input.txt +RTS -s`
#### Genetic Algorithm with Population Size and Number of Rounds
`stack exec tsp-functional-exe -- -g :s64 :r128 /Users/jcs/projects/tsp-functional/input.txt +RTS -s`

