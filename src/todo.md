# show dump string of BitStruct

## show short/long
show in REPL should give long version, currently in gives 1-line-version
for a struct, show and string return 1-line

## typename
BitStruct{T} default printout prints too much. "BitStruct" without parameters prints currently not enough

Idea: 
 - redefine zuas BitStruct{N,S,T} where N (const symbol) S (symbol tuple) T (type tuple)
 - use N as short type name
 - overload print(BitStruct{...}) to print only N: use debugger to track -> method to overload

# method parameters

methods can have Union{BS1,BS2...} as param type, assuming BS1 and BS2 share field names !!
(DOC only)

# IO

## methods like iterators: state==(open bits,IO)

read, write with bit buffer as additional parameter (helper type), using any IO
not: derived IO because: highly inefficient to read byteoriented data from bitstream

# test

## file struct

commontypes.jl: benchmark& test data types, to include
testio, testops: include in runtests
benchmark: 3 simple files. 

## benchmark
structure, reporting, save/compare??!
 - julia version
 - 


## optimize

del subfields
del & | vs && || comments

try "all inbounds"
replace @boundscheck by @ondebug controlled by a global constant?