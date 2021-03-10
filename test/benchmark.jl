
using BitStructs
using BenchmarkTools

@enum ProcStatus ::UInt8 S_WAITING S_RUNNING S_DONE S_FAILURE

@enum Strange ::Int BIGMINUS=-999999 ONE=1 TWO=2 THREE=3
BitStructs.bitsizeof(::Type{Strange}) = 2
BitStructs.encode(v::Strange) = (v==BIGMINUS ? zero(UInt64) : Int(v)%UInt64)
BitStructs.decode(::Type{Strange},u::UInt64) = (u==zero(UInt64) ? BIGMINUS : StrangeEncoded(u))

struct Sign end
BitStructs.bitsizeof(::Type{Sign}) = 1
BitStructs.encode(::Type{Sign},v::Float64) = (v.value>0 ? zero(UInt64) : one(UInt64))
BitStructs.decode(::Type{Sign},u::UInt64) = (u==zero(UInt64) ? 1.0 : -1.0)



"""
BS is a BitStruct designed to demonstrate BitStruct advantages.

As such, it has many small fields and two "pathological" cases with the
types Strange and Sign, taken from tutorial.jl. Efficiency oriented 
programmers would probably avoid  
"""
@bitstruct BS begin
    status :: ProcStatus # could be the overall status of some process
    strange :: Strange 
    sign :: Sign
    flag1 :: Bool
    flag2 :: Bool
    flag3 :: Bool # to be honest: many flags push memory savings and runtime advantages of this BitStruct
    flag4 :: Bool # you could always use Bool for a one-bit field, but ...
    bit1  :: BInt{1} # in a numerical context, BInt/BUInt does the number conversion for you
    ac :: AsciiChar # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    lc :: Latin1Char # similar use 8 bits for the Latin-1 character set
    id1 :: BUInt{9} # 0..511
    id2 :: BUInt{12} # 0..4095
    delta1 :: BInt{9} # -256..255
    delta2 :: BInt{9} # -256..255 
end

"""
This struct has fields with the same names as BS and field types
compatible to the types in BS with the smallest size available in
standard julia types to cover the encoded value range. 

"""
struct S begin
    status :: UInt8 # could be the overall status of some process
    strange :: Int32
    sign :: Float16 # typeof(1.0)===Float64, but that would be overkill ;-) 
    flag1 :: Bool
    flag2 :: Bool
    flag3 :: Bool # to be honest: many flags push memory savings and runtime advantages of this BitStruct
    flag4 :: Bool # you could always use Bool for a one-bit field, but ...
    bit1  :: Int8 # in a numerical context, BInt/BUInt does the number conversion for you
    ac :: Char # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    lc :: Char # similar use 8 bits for the Latin-1 character set
    id1 :: UInt16 # 0..511
    id2 :: UInt32 # 0..4095
    delta1 :: Int16 # -256..255
    delta2 :: Int16 # -256..255 
end