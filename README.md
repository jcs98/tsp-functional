# A functional implementation to solve the Traveling Salesman Problem

## Setup
`stack setup`

## Build and Run    
`stack build`    
`stack exec tsp-functional-exe [-s|-p|-cN] <filename>`    

or

`stack build --profile`    
`stack exec tsp-functional-exe --profile -- [-s|-p|-cN] <filename> +RTS -N4 -s -p -ls`    
(-s: spark info, -p: profiling, -ls: eventlog)    


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