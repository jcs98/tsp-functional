# tsp-functional

## Steps to run
Step 1. `stack setup`

Step 2.    
`stack build`    
`stack exec tsp-functional-exe [-s|-p|-cN] <filename>`    

or

`stack build --profile`    
`stack exec tsp-functional-exe --profile -- [-s|-p|-cN] <filename> +RTS -N4 -s -p -ls`    
(s: spark info, p: profiling, ls: eventlog)    

or

`stack run`

Step 3. `stack install`
