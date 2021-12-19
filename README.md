# A functional implementation to solve the Traveling Salesman Problem

## Setup
`stack setup`

## Build and Run    
`stack build`    
`stack exec tsp-functional-exe [-s|-p|-c :nN|-s :bN|-sp :bN|-g :pN :gN|-g :pN :gN :bN|-gp :pN :gN :bN] <filename>`    

or

`stack build --profile`    
`stack exec tsp-functional-exe --profile -- [-s|-p|-c :nN|-s :bN|-sp :bN|-g :pN :gN|-g :pN :gN :bN|-gp :pN :gN :bN] <filename> +RTS -N4 -s -p -ls`    
(-s: spark info, -p: profiling, -ls: eventlog)    
(N = any integer, :b for batch size)    

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

#### Bruteforce sequential
`stack exec tsp-functional-exe -- -s /Users/jcs/projects/tsp-functional/input.txt`
#### Bruteforce, calculate path distance in parallel
`stack exec tsp-functional-exe -- -p /Users/jcs/projects/tsp-functional/input.txt +RTS -N4`
#### Bruteforce, calculate path distance in parallel with Chunk size
`stack exec tsp-functional-exe -- -c :n1024 /Users/jcs/projects/tsp-functional/input.txt +RTS -N4`
#### Bruteforce for batch of city groups
`stack exec tsp-functional-exe -- -s :b128 /Users/jcs/projects/tsp-functional/input.txt`
#### Bruteforce for batch of city groups, each group in parallel
`stack exec tsp-functional-exe -- -sp :b128 /Users/jcs/projects/tsp-functional/input.txt +RTS -N4`

#### Genetic Algorithm with Population Size and Number of Generations
`stack exec tsp-functional-exe -- -g :p64 :g128 /Users/jcs/projects/tsp-functional/input.txt`
#### Genetic Algorithm for batch of city groups
`stack exec tsp-functional-exe -- -g :p64 :g128 :b10 /Users/jcs/projects/tsp-functional/input.txt`
#### Genetic Algorithm for batch of city groups, each group in parallel
`stack exec tsp-functional-exe -- -gp :p64 :g128 :b10 /Users/jcs/projects/tsp-functional/input.txt +RTS -N4`

