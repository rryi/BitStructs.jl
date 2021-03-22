using BitStructs
using Random
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
An usual Enum, and very well suited for BitStructs
"""
@enum ProcStatus ::UInt8 S_WAITING S_RUNNING S_DONE S_FAILURE


"""
A not so well defined Enum, but very well suited for BitStructs with customized encoding.
"""
@enum Strange ::Int BIGMINUS=-999999 ONE=1 TWO=2 THREE=3
BitStructs.bitsizeof(::Type{Strange}) = 2
BitStructs.encode(v::Strange) = (v==BIGMINUS ? zero(UInt64) : Int(v)%UInt64)
BitStructs.decode(::Type{Strange},u::UInt64) = (u==zero(UInt64) ? BIGMINUS : Strange(u))

"""
An example of a custom bit type in the Float64 domain.
"""
struct Sign end
BitStructs.bitsizeof(::Type{Sign}) = 1
BitStructs.encode(::Type{Sign},v::Float64) = (v>=0.0 ? zero(UInt64) : one(UInt64))
BitStructs.decode(::Type{Sign},u::UInt64) = (u==zero(UInt64) ? 1.0 : -1.0)


@bitstruct BFlags begin
    f1::Bool
    f2::Bool
    f3::Bool    
end

"""
BSArg is a small argument list, as demo of something which could be a kernel set of
parameters used in many functions of a problem domain
"""
@bitstruct BSArg begin # has 24 bits
    f       :: BFlags
    state   :: ProcStatus
    id      :: BUInt{10} # 0..1023
    delta   :: BInt{9} # -256..255
end


@bitstruct BS begin
    strange :: Strange 
    sign    :: Sign
    f       :: BFlags
    state   :: ProcStatus
    bit     :: BInt{1} # in a numerical context, BInt/BUInt does the number conversion for you. bit can be -1, 0
    ac      :: AsciiChar # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    first   :: BSArg # field name allows this type to be used like a Pair
    second  :: BSArg # field name allows this type to be used like a Pair
end


# and now "the same" as julia struct

"""
IFlags and its BitStruct counterpart BFlags encapsulate several flags, 
they are used as field type in other structs/BitStructs, 
to allow for AND / OR operations on the whole group of flags. This enables fast
checks if all flags are set or unset. 

We will benchmark with it whether "short circuit" boolean AND/OR using && and || are
really "short" in runtime, if the expressions to evaluate are simple values.

The expectation is, that modern CPU architectures turn short circuit into long
circuit it such cases, because unexpected branching causes a complete
reload of the CPU opcode pipeline - much more expensive that a couple of
operations on registers. 
"""
struct IFlags 
    f1::Bool
    f2::Bool
    f3::Bool    
end

function BitStructs.set(x::IFlags, field::Symbol, value)
    if field===:f1
        return ISArg(value,x.f2,x.f3)
    elseif field===:state
        return ISArg(x.f1,value,x.f3)
    elseif field===:id
        return ISArg(x.f1,x.f2,value)
    end
    throw(ArgumentError("unknown field name: $field"))
end


"""
ISArg respective its BitStruct counterpart BSArg is a small argument list, 
as demo of something which could be a code set of
parameters (re-)used in many methods of a problem domain.

Using the restricted Integer types with 16 bits, this struct should fit into 8 bytes.
Benchmarks will show, if it is as efficient as BSArg when changing field values
in an ISArg variable.
"""
struct ISArg
    f       :: IFlags
    state   :: ProcStatus
    id      :: UInt16 # 0..1023
    delta   :: Int16 # -256..255
end


# for field changes, we need some setters on the IS* structs.
# API is designed as for BitStruct-S
function BitStructs.set(x::ISArg, field::Symbol, value)
    if field===:f
        return ISArg(value,x.state,x.id,x.delta)
    elseif field===:state
        return ISArg(x.f,value,x.id,x.delta) 
    elseif field===:id
        return ISArg(x.f,x.state,value,x.delta) 
    elseif field===:delta
        return ISArg(x.f,x.state,x.id,value) 
    end
    throw(ArgumentError("unknown field name: $field"))
end

"""
IS and its BitStruct counterpart BS are designed to demonstrate BitStruct advantages.

As such, it has many small fields and two fields for data types which were not designed 
for memory efficiency (Strange, Sign), demonstrating how a custom encoding in a BitStruct
can overcome memory deficiencies.

There is a mutable variant of IS called MS, because changing a field 
of a variable v::IS is expensive, due to its size: it requires copying 
all other fields. 

For a BitStruct, which is always immutable, changing a field of a variable 
v::BS also requires copying all other fields, but this done with extreme 
efficiency by bit mask operations, working on a machine register
(in a 64 bit julia version).

To get realistic benchmark results, we compare BS with MS in scenarios 
with frequent field changes. Most julia programmers will choose a mutable 
struct if field changes are expected - last not least for convenience: 
julia has no neat syntax for replacing a field in a variable of an 
immutable struct type, but it has for mutable structs.
"""
struct IS 
    strange     :: Strange 
    sign        :: Float16
    f           :: IFlags
    state       :: ProcStatus
    bit         :: Int8 # in a numerical context, BInt/BUInt does the number conversion for you. bit can be -1, 0
    ac          :: Char # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    first       :: ISArg # field name allows this type to be used like a Pair
    second      :: ISArg # field name allows this type to be used like a Pair
end


mutable struct MS
    strange     :: Strange 
    sign        :: Float16
    f           :: IFlags
    state       :: ProcStatus
    bit         :: Int8 # in a numerical context, BInt/BUInt does the number conversion for you. bit can be -1, 0
    ac          :: Char # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    first       :: ISArg # field name allows this type to be used like a Pair
    second      :: ISArg # field name allows this type to be used like a Pair
end

# for field changes, we need some setters on the IS* structs.
# API is designed as for BitStruct-S
function BitStructs.set(x::IS, field::Symbol, value)
    if field===:strange
        return IS(value,x.sign,x.f,x.state,x.bit,x.ac,x.lc)
    elseif  field===:sign
        return IS(x.strange,value,x.f,x.state,x.bit,x.ac,x.first,x.second)
    elseif  field===:f
        return IS(x.strange,x.sign,value,x.state,x.bit,x.ac,x.first,x.second)
    elseif  field===:state
        return IS(x.strange,x.sign,x.f,value,x.bit,x.ac,x.first,x.second)
    elseif  field===:bit
        return IS(x.strange,x.sign,x.f,x.state,value,x.ac,x.first,x.second)
    elseif  field===:ac
        return IS(x.strange,x.sign,x.f,x.state,x.bit,value,x.first,x.second)
    elseif  field===:first
        return IS(x.strange,x.sign,x.f,x.state,x.bit,x.ac,value,x.second)
    elseif  field===:second
        return IS(x.strange,x.sign,x.f,x.state,x.bit,x.ac,x.first,value)
    end
    throw(ArgumentError("unknown field name: $field"))
end

function BitStructs.set(x::MS, field::Symbol, value)
    setproperty!(x,field,value)
    return x
end

function fieldcopy(::Type{T},src::U) where {T,U}
    args = Any[]
    for s in fieldnames(U)
        fld = getproperty(src,s)
        if typeof(fld) <: BitStruct
            FT = fieldtype(T,s)
            fld = fieldcopy(FT,fld)
        end
        push!(args,fld)
    end
    return T(args...)
end



#@code_native BitStructs._fielddescr(BS,Val(:status))

BitStructs.generate(BFlags)#,false,false)
BitStructs.generate(BSArg)#,false,false)
BitStructs.generate(BS)#,false,false)


const vecsize = 100 # vector size for benchmark loop
rng = MersenneTwister(1234);
uv = Vector{UInt64}(undef,vecsize)
rand!(rng,uv)
bsv = BS[]
isv = IS[]
msv = MS[]

#bs = reinterpret(BS,uv[1])
#is = fieldcopy(IS,bs)

for i = 1:vecsize
    local bs = reinterpret(BS,uv[i])
    push!(bsv,bs)
    local is = fieldcopy(IS,bs)
    push!(isv,is)
    local ms = fieldcopy(MS,bs)
    push!(msv,ms)
end



prompt("foo flag or: test with || if any flag is set in loop mutable struct/struct/BitStruct/ /=")
@noinline function foo(b::Vector{T}) where T
    s = 0
    for st in b
        if st.f.f1 || st.f.f2 || st.f.f2
            s += 1
        end
    end        
    return s
end
@btime (foo($msv))
@btime (foo($isv))
@btime (foo($bsv))
prompt("fo flag or: test with | if any flag is set in loop mutable struct/struct/BitStruct/ /=")
@noinline function fo(b::Vector{T}) where T
    cmp = typeof(b[1].f)(false,false,false)
    s = 0
    for i in 1:length(b)
        st = b[i]
        if st.f === cmp
            s += 1
        end
    end        
    return s
end
@btime (fo($msv))
@btime (fo($isv))
@btime (fo($bsv))
# TODO reason for alloc-s in BitStruct case ??? 



prompt("w0 write subfields in loop mutable struct/struct/BitStruct")
@noinline function w0(b::Vector{T}) where T
    s = 0%UInt64
    for i in one(UInt64):UInt64(100)
        b[i] = set(b[i],:first, set(b[i].first,:id,b[i].second.id))
    end        
    return b
end
@btime (w0($msv))
@btime (w0($isv))
@btime (w0($bsv))

prompt("b0 read subfields in loop mutable struct/struct/BitStruct")
@noinline function b0(b::Vector{T}) where T
    s = 0%UInt64
    for i in one(UInt64):UInt64(100)
        s += b[i].first.id + b[i].second.id
    end        
    return s
end
@btime (b0($msv))
@btime (b0($isv))
@btime (b0($bsv))

## method param benchmark


@inline function work(v1::Vector{Int}, v2::Vector{Int}, s) # s is a ?S
    v1[1] = v2[1] # just to force use of v1,v2 
    if (s.f.f1 || s.f.f2) && (s.first.f.f3 || s.second.f.f3)
        s = set(s,:bit,-1)
        s = set(s,:first, s.second)
    else
        s = set(s,:bit,0)
        s = set(s,:second, s.first)
    end
    v2[2] = v1[2]
    s = set(s,:sign,Float64(v1[3]-v2[3]))
    return subWork(v1,v2,s.first)+subWork(v2,v1,s.second)
end

@inline function subWork(v1::Vector{Int}, v2::Vector{Int}, s) # s is a ?SArg
    v2[1] = v1[3] # just to force use of v1,v2 
    if s.f.f1
        s = set(s, :id, (abs(s.delta)+v2[3])%UInt )
    else
        s = set(s, :delta, s.id/2 -v2[3])
    end
    if s.state == S_RUNNING 
        s = set(s, :state, S_DONE)
    else
        s = set(s, :state, S_FAILURE)
    end
    return s.id+s.delta
end


v1 = [1,2,3]
v2 = [4,5,6]

prompt("function parameter test mutable struct/struct/BitStruct")
@btime work($v1,$v2,$msv[1])
@btime work($v1,$v2,$isv[1])
@btime work($v1,$v2,$bsv[1])


