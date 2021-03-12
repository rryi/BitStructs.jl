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

"""
This struct has fields with the same names as BS and field types
compatible to the types in BS with the smallest size available in
standard julia types to cover the encoded value range. 

It is declared mutable for two reasons: 
    (a) it is quite large - 28 bytes
    (b) it contains several fields which are due to frequent updates



"""
mutable struct S 
    status :: ProcStatus # could be the overall status of some process
    strange :: Strange
    sign :: Float16 # typeof(1.0)===Float64, but that would be overkill ;-) 
    flag1 :: Bool
    flag2 :: Bool
    flag3 :: Bool # to be honest: many flags push memory savings and runtime advantages of this BitStruct
    flag4 :: Bool # you could always use Bool for a one-bit field, but ...
    bit1  :: Int8 # in a numerical context, BInt/BUInt does the number conversion for you
    ac :: Char # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    lc :: Char # similar use 8 bits for the Latin-1 character set
    id1 :: UInt16 # 0..511
    id2 :: UInt16 # 0..4095
    delta1 :: Int16 # -256..255
    delta2 :: Int16 # -256..255 
end
Base.copy(x::T) where T<:S = T([getfield(x, k) for k ∈ fieldnames(T)]...)



v1 = [1,2,3]
v2 = [4,5,6]

s = S(S_RUNNING,BIGMINUS,Float16(-1.0),true,false,false,true,0%Int8,'a','c',0x0001,0x0002,3%Int16,4%Int16)
bs = BS(S_RUNNING,BIGMINUS,-1.0,true,false,false,true,0,'a','c',0x1,0x2,3,4)

show(s)

show(bs)





## some simple field access benchmarks to begin with

mutable struct T
    id1::UInt16
    id2::UInt16
    flag1::Bool
    flag2::Bool
end
Base.copy(x::T) = T([getfield(x, k) for k ∈ fieldnames(T)]...)

@bitstruct BT begin
    id1::BUInt{16}
    id2::BUInt{16}
    flag1::Bool
    flag2::Bool
end

t = T(5%UInt16, 6%UInt16, true, false)
bt = BT(5%UInt16, 6%UInt16, true, false)


prompt("struct/BitStruct simple field access")
#@btime $t.id1,$t.id2,$t.flag1,$t.flag2
# this is optimized away, totally
#@btime $bt.id1,$bt.id2,$bt.flag1,$bt.flag2


prompt("struct/BitStruct field access in @noinline function")
@noinline bench1(t) = (t.id1+t.id2,t.flag1,t.flag2)
@btime bench1($t)
@btime bench1($bt)

@noinline function set2fields(s)
    if typeof(s) <: BitStruct
        s /= :id1, s.id2
        s /= :flag1, s.flag2
    else
        s.id1 = s.id2
        s.flag1 = s.flag2
    end
end


tc = copy(t)
prompt("set 2 fields on struct then BitStruct")
#@btime set2fields($tc)
#@btime set2fields($bt)


sc = copy(s)
prompt("set 2 fields on struct then BitStruct (large struct)")
#@btime set2fields($sc)

println("the statements executed by set2fields run, if executed directly")
bs /= :id1, s.id2
bs /= :flag1,bs.flag2
show(bs)
println("Calling set2fields(bs) causes a crash")
set2fields(bs)
# !!!!!!!!!!!!!!!!!!!!!!!! next line crashes process !!!!!!!!!!!!!!!!!!!!!
#set2fields(bs)
#@btime set2fields($bs)
# drill down on time consume setting a field

