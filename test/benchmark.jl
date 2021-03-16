using BitStructs
using Random
using BenchmarkTools

rng = MersenneTwister(1234);

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


#@code_native BitStructs._fielddescr(BS,Val(:status))

specialize(BS)

@code_native BitStructs._fielddescr(BS,Val(:status))


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

"""
    workOnS(v1::Vector, v2::Vector,args::S)

    The benchmark function concerning runtime efficiency for BitStructs which
    encapsulate many function parameters.
    
    workOnS has 3 methods, having the same functionality, but different parameter solutions. 
    One has an ordinary parameter list. One has a a conventional struct of type S,
    which bundles those parameters, and one has a BitStruct of type BS doing the same.

    workOnS calls a function subWorkOnS, having a subset of the parameters (50%). For structs,
    a (copy of) the struct is used as parameter, for a calling list, all parameters are given
    as positional parameters. 
    
    In practical use cases, such functions will be quite large, due to its many parameters,
    too large for inlining its code. To simulate this in the benchmark, they are
    declared @noinline.
"""
function workOnS end

function subWorkOnS end


@inline function workOnS(v1::Vector{Int}, v2::Vector{Int}, s::BS)
    v1[1] = v2[1] # just to force use of v1,v2 
    if (s.flag1 || s.flag2) && (s.flag3 || s.flag4)
        s /= (:bit1,-1)
        s /= :delta1, 15 - s.delta1 
    else
        s /= :bit1,0
        s /= :delta2, 51 - s.delta2
    end
    v2[2] = v1[2]
    s /= :id2, (s.id2 - s.id1 + s.delta2)
    return subWorkOnS(v1,v2,s)
end

@inline function subWorkOnS(v1::Vector{Int}, v2::Vector{Int}, s::BS)
    v2[1] = v1[3] # just to force use of v1,v2 
    if s.bit1==-1
        s /= :id1, abs(s.delta1 - s.delta2) %UInt64
    else
        s /= :id1, abs(s.delta2 - s.delta1) %UInt64
    end
    s /= :sign , Float64(s.delta1 * s.delta2) # projection on 1.0/-1.0 is done by encode
    v1[2] = v2[3]
    if s.status == S_RUNNING && s.sign < 0.0
        s /= :status, S_DONE
    else
        s /= :status, S_FAILURE
    end
    return s
end


@inline function workOnS(v1::Vector{Int}, v2::Vector{Int}, s::S)
    v1[1] = v2[1] # just to force use of v1,v2 
    if (s.flag1 || s.flag2) && (s.flag3 || s.flag4)
        s.bit1 = -1
        s.delta1 = 15 - s.delta1 
    else
        s.bit1 = 0
        s.delta2 = 51 - s.delta2
    end
    v2[2] = v1[2]
    s.id2 = (s.id2 - s.id1 + s.delta2)
    return subWorkOnS(v1,v2,s) 
end

@inline function workOnScopy(v1::Vector{Int}, v2::Vector{Int}, s::S)
    v1[1] = v2[1] # just to force use of v1,v2 
    if (s.flag1 || s.flag2) && (s.flag3 || s.flag4)
        s.bit1 = -1
        s.delta1 = 15 - s.delta1 
    else
        s.bit1 = 0
        s.delta2 = 51 - s.delta2
    end
    v2[2] = v1[2]
    s.id2 = (s.id2 - s.id1 + s.delta2)
    return subWorkOnS(v1,v2,copy(s))
end



@inline function subWorkOnS(v1::Vector{Int}, v2::Vector{Int}, s::S)
    v2[1] = v1[3] # just to force use of v1,v2 
    if s.bit1==-1
        s.id1 = abs(s.delta1 - s.delta2) %UInt64
    else
        s.id1 = abs(s.delta2 - s.delta1) %UInt64
    end
    s.sign = Float16(s.delta1 * s.delta2>= 0.0 ? 1.0 : -1.0) # we must code projection here
    v1[2] = v2[3]
    if s.status == S_RUNNING && s.sign < 0.0
        s.status = S_DONE
    else
        s.status = S_FAILURE
    end
    return s
end



@inline function workOnS(v1::Vector{Int}, v2::Vector{Int}, 
    s_status :: ProcStatus,
    s_strange :: Strange,
    s_sign :: Float16,
    s_flag1 :: Bool,
    s_flag2 :: Bool,
    s_flag3 :: Bool,
    s_flag4 :: Bool,
    s_bit1  :: Int8,
    s_ac :: Char,
    s_lc :: Char,
    s_id1 :: UInt16,
    s_id2 :: UInt16,
    s_delta1 :: Int16,
    s_delta2 :: Int16)

    v1[1] = v2[1] # just to force use of v1,v2 
    if (s_flag1 || s_flag2) && (s_flag3 || s_flag4)
        s_bit1 = -1 %Int8
        s_delta1 = (15 - s_delta1 )%Int16
    else
        s_bit1 = 0%Int8
        s_delta2 = (51 - s_delta2) %Int16
    end
    v2[2] = v1[2]
    s_id2 = (s_id2 - s_id1 + s_delta2)
    return subWorkOnS(v1,v2,s_status,s_sign,s_bit1,s_id1,s_delta1,s_delta2)
end


@inline function subWorkOnS(v1::Vector{Int}, v2::Vector{Int}, 
    s_status :: ProcStatus,
    s_sign :: Float16,
    s_bit1  :: Int8,
    s_id1 :: UInt16,
    s_delta1 :: Int16,
    s_delta2 :: Int16)
    
    v2[1] = v1[3] # just to force use of v1,v2 
    if s_bit1==-1
        s_id1 = abs(s_delta1 - s_delta2)
    else
        s_id1 = abs(s_delta2 - s_delta1)
    end
    s_sign = Float16(s.delta1 * s.delta2 >= 0.0 ? 1.0 : -1.0) # we must code projection here
    v1[2] = v2[3]
    if s_status == S_RUNNING && s_sign < 0.0
        s_status = S_DONE
    else
        s_status = S_FAILURE
    end
    return s_status,s_sign,s_id1 # return all changed values
end


v1 = [1,2,3]
v2 = [4,5,6]

s = S(S_RUNNING,BIGMINUS,Float16(-1.0),true,false,false,true,0%Int8,'a','c',0x0001,0x0002,3%Int16,4%Int16)
bs = BS(S_RUNNING,BIGMINUS,-1.0,true,false,false,true,0,'a','c',0x1,0x2,3,4)

#show(s)

#show(bs)





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
specialize(BT)

t = T(5%UInt16, 6%UInt16, true, false)
bt = BT(5%UInt16, 6%UInt16, true, false)


prompt("struct/BitStruct simple field access")
@btime $t.id1,$t.id2,$t.flag1,$t.flag2
# this is optimized away, totally
@btime $bt.id1,$bt.id2,$bt.flag1,$bt.flag2


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
    return s
end


tc = copy(t)
prompt("set 2 fields on struct then BitStruct")
@btime set2fields($tc)
@btime set2fields($bt)


@noinline function setSpecialfields(s::T,s2::T) where T <: Union{S,BS}
    if typeof(s) <: BitStruct
        s /= :status, s2.status
        s /= :strange, s2.strange
        s /= :sign, s2.sign
    else
        s.status = s2.status
        s.strange = s2.strange
        s.sign = s2.sign
    end
    return s
end


sc = copy(s)
s2 = copy(s)
prompt("set special fields on struct then BitStruct")
@btime setSpecialfields($sc,$s2)
@btime setSpecialfields($bs,$bs)


sc = copy(s)
prompt("set 2 fields on struct then BitStruct (large struct)")
@btime set2fields($sc)

println("the statements executed by set2fields run, if executed directly")
bs /= :id1, s.id2
bs /= :flag1,bs.flag2
#show(bs)
println("Calling set2fields(bs) causes a crash")
set2fields(bs)
# !!!!!!!!!!!!!!!!!!!!!!!! next line crashes process !!!!!!!!!!!!!!!!!!!!!
#set2fields(bs)
@btime set2fields($bs)
# drill down on time consume setting a field



prompt("b1 set field in loop by /=")
@noinline function b1(b::T) where T <: BitStruct
    s = 0%UInt64
    for i in zero(UInt64):UInt64(100)
        b /= (:id1, i)
        s += b.id1
    end        
    return s
end
@btime (b1($bt))


prompt("b2 set field in loop by set(..)")
@noinline function b2(b::T) where T <: BitStruct
    s = 0%UInt64
    for i in zero(UInt64):UInt64(100)
        b = BitStructs.set(b,:id1, i)
        s += b.id1
    end        
    return s
end
@btime (b2($bt))




btv = BT[]
tv =  T[]


for i in 1:100
    push!(tv,T(i%UInt16,(i+1)%UInt16,true,false))
    push!(btv,BT(i%UInt16,(i+1)%UInt16,true,false))
end


"access 4 fields in a loop"
function bench1(vec)
    sum = 0%UInt64
    for t in vec
        sum += t.id1+t.id2+UInt(t.flag1)+UInt(t.flag2)
    end
    sum
end

"write 4 fields in a loop"
function bench2(vec)
    sum = 0
    if eltype(vec) <: BitStruct
        for i in 2:length(vec)
            t = vec[i]
            t = t / (:id1 , i%UInt64) / (:id2 , i%UInt64) / (:flag1 , i%2 ==0) / (:flag2 , i%2 !=0)
            sum += t.id1
            vec[i-1] = t
        end
    else
        for i in 2:length(vec)
            t = vec[i]
            t.id1 = i%UInt64
            t.id2 = i%UInt64
            t.flag1 = (i%2 ==0)
            t.flag2 = (i%2 !=0)
            sum += t.id1
            vec[i-1] = t
        end
    end
    sum
end

prompt("struct/BitStruct: access 4 fields in a loop")
@btime bench1($tv)
@btime bench1($btv)

prompt("struct/BitStruct: write 4 fields in a loop")
@btime bench2($tv)
#crash!
#bench2(btv)
@btime bench2($btv)



bsv = BS[]
sv =  S[]


for i in 1:100
    push!(sv,s)
    push!(bsv,bs)
end


prompt("struct/BitStruct: access 4 fields in a loop, large struct")
@btime bench1($sv)
@btime bench1($bsv)

prompt("struct/BitStruct: write 4 fields in a loop, large struct")
@btime bench2($sv)
#crash!
#bench2(bsv)
@btime bench2($bsv)





# currently, bad again... no constant propagation.
prompt("struct/BitStruct: access 4 fields, direct code")
@btime $s.delta1-$s.delta2,$s.id1, $s.id2
@btime $bs.delta1-$bs.delta2,$bs.id1, $bs.id2







## function parameter list benchmark
prompt("function parameter benchmark: struct/structWithCopy/BitStruct/parameterlist")
@btime workOnS($v1,$v2,$s)
@btime workOnScopy($v1,$v2,$s)
#crash!
#workOnS(v1,v2,bs)
@btime workOnS($v1,$v2,$bs)

@btime workOnS($v1,$v2,S_RUNNING,BIGMINUS,Float16(-1.0),true,false,false,true,0%Int8,'a','c',0x0001,0x0002,3%Int16,4%Int16)
nothing

