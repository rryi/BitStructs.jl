using BitStructs
using BenchmarkTools

#=
module BitStructs
function bitsizeof end
function encode end
function decode end
end
=#

function prompt(p::AbstractString="")
    println(p, " ENTER: ")
    #return chomp(readline())
end


"""
An usual Enum, and very well suited für BitStructs
"""
@enum ProcStatus ::UInt8 S_WAITING S_RUNNING S_DONE S_FAILURE


"""
A not so well defined Enum, but very well suited für BitStructs with customized encoding.
"""
@enum Strange ::Int BIGMINUS=-999999 ONE=1 TWO=2 THREE=3
BitStructs.bitsizeof(::Type{Strange}) = 2
BitStructs.encode(v::Strange) = (v==BIGMINUS ? zero(UInt64) : Int(v)%UInt64)
BitStructs.decode(::Type{Strange},u::UInt64) = (u==zero(UInt64) ? BIGMINUS : StrangeEncoded(u))

"""
An example of a custom bit type in the Float64 domain.
"""
struct Sign end
BitStructs.bitsizeof(::Type{Sign}) = 1
BitStructs.encode(::Type{Sign},v::Float64) = (v>=0.0 ? zero(UInt64) : one(UInt64))
BitStructs.decode(::Type{Sign},u::UInt64) = (u==zero(UInt64) ? 1.0 : -1.0)





"""
BS is a BitStruct designed to demonstrate BitStruct advantages.

As such, it has many small fields and two fields for data types which were not designed 
for memory efficiency (Strange,Sign), demonstrating how a custom encoding can overcome memory deficiencies.
"""
@bitstruct BS begin
    status :: ProcStatus # could be the overall status of some process
    strange :: Strange 
    sign :: Sign
    flag1 :: Bool
    flag2 :: Bool
    flag3 :: Bool # to be honest: many flags push memory savings and runtime advantages of this BitStruct
    flag4 :: Bool # you could always use Bool for a one-bit field, but ...
    bit1  :: BInt{1} # in a numerical context, BInt/BUInt does the number conversion for you bit1 can be -1, 0
    ac :: AsciiChar # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    lc :: Latin1Char # similar use 8 bits for the Latin-1 character set
    id1 :: BUInt{9} # 0..511
    id2 :: BUInt{12} # 0..4095
    delta1 :: BInt{9} # -256..255
    delta2 :: BInt{9} # -256..255 
end


println(methods(BitStructs._fielddescr))
@code_native BitStructs._fielddescr(BS,Val(:status))

specialize(BS)

println(methods(BitStructs._fielddescr))
@code_native BitStructs._fielddescr(BS,Val(:status))

_fielddescr(BitStruct{NamedTuple{(:status, :strange, :sign, :flag1, :flag2, :flag3, :flag4, :bit1, :ac, :lc, :id1, :id2, :delta1, :delta2), 
Tuple{ProcStatus, Strange, Sign, Bool, Bool, Bool, Bool, BInt{1}, AsciiChar, Latin1Char, BUInt{9}, BUInt{12}, BInt{9}, BInt{9}}}}, Val(:ac))
