# Type BitStruct and its operations


"""
BitStruct ist the central type of package BitStructs.

A BitStruct is a primitive 64-bit type which supports properties 
similar to a (immutable) julia struct type.

In contrast to a julia struct, its fields are aligned at 
bit boundaries in the BitStruct container, which reduces
memory consumption for suitable field types at a very small price 
(a >>> and a & on an UInt64 value). The biggest memory gain is 
achieved for Bool fields (factor 8), Enum-s with a few instances
and 

BitStruct subtypes should always be defined using macro
[`@bitstruct`](@ref), which comes syntactically close
to a usual julia struct declaration. It does essential checks
on the valitdity of BitStruct type - it is easy to define
a NamedTuple Type which leads to an invalid BitStruct. 
"""
primitive type BitStruct{T<:NamedTuple} 64 end


Base.fieldnames(::Type{BitStruct{T}}) where T = T.parameters[1]
Base.propertynames(ps::BitStruct{T}) where T = fieldnames(BitStruct{T})

# NTuple{N, Any} is supertype of all tuples of length N!
# tuple_len(::NTuple{N, Any}) where {N} = Val{N}()


"""
_mask(Int::bits) :: UInt64

return a bit mask to restrict to the lowest *bits* bits of an UInt64.
"""
@inline _mask(bits) = one(UInt64)<<bits - one(UInt64)


"""
    _get(pstruct::UInt64, shift, bits)

extract a bitfield from a packed struct.
If pstruct is interpreted as a bit vector, it returns pstruct[shift+1:shift+bits] 
"""
@inline _get(pstruct::UInt64, shift, bits) = (pstruct>>>shift) & _mask(bits)

# this variant might give better code (guaranteed constant propagation)
@inline _get(pstruct::UInt64, ::Val{shift},::Val{bits}) where {shift,bits} = (pstruct>>>shift) & _mask(bits)



"""
    _set(pstruct::UInt64, shift, bits, value::UInt64)

set a bitfield in a packed struct.
If pstruct and value are interpreted as bit vector, it performs pstruct[shift+1:shift+bits] = value[1..bits]

Boundscheck tests if no bit is set in balue except the lowest *bits* bits. 
This is guaranteed by encode(...,bits).
"""
function _set(pstruct::UInt64,shift,bits,value::UInt64) 
    @boundscheck checkbitsize(value,bits)
    pstruct &= !(_mask(bits) << shift) # delete bitfield
    pstruct |= value << shift
    return pstruct
end

# variant for benchmarking...
@inline function _set(pstruct::UInt64,::Val{shift},::Val{bits}, value::UInt64) where {shift, bits}
    @boundscheck checkbitsize(value,bits)
    pstruct &= ~(_mask(bits) << shift) # delete bitfield
    pstruct |= value << shift
    return pstruct
end



"""
    function _fielddescr(::Type{BitStruct{T}},s::Symbol)

extract field descriptor (type,shift,bits) from type info for a symbol S.
If S is not found, (Nothing,0,0) is returned.

This function suffers from "new method in old world" problem: any type defined after its first use
will cause an exception if the type is used in a BitStruct. Besides that, it is ultrafast,
returning a constant

workaround for benchmarking: change this code after defining all types.
automatic recompile will fix it for that run.

bitsizeof on Enum uses typemax, seems invokelatest is not recursive wrt world.
ERROR: LoadError: MethodError: no method matching typemax(::Type{ProcStatus})
The applicable method may be too new: running in world age 29624, while current world is 29656.
Closest candidates are: 
  typemax(::Type{ProcStatus}) at Enums.jl:197 (method too new to be called from this world context.)
"""
function _fielddescr end
#=
function _fielddescr(::Type{BitStruct{T}},::Val{s}) where {T<:NamedTuple,s} # s isa Symbol
    shift = 0
    types = T.parameters[2].parameters
    syms = T.parameters[1]
    idx = 1
    while idx <= length(syms)
        type = types[idx]
        #bits = Int(Base.invokelatest(bitsizeof,type)) # bitsizeof must be recursively defined with invokelatest!!!
        bits = bitsizeof(type)
        if syms[idx]===s
            # @generated: return :(($type,$shift,$bits))
            return type,shift,bits
        end
        shift += bits
        idx += 1
    end
    throw(ArgumentError(s)) 
end
=#

# constant propagation did work in this recursive formulation, but is fragile ... 
# some changes later, it was lost
# even worse: crash due to compiler confusion caused by strange tuple type Tuple{:a,:b...}

@inline function __fielddescr(::Type{BitStruct{T}},::Val{s}) where {T<:NamedTuple,s}
    _fielddescr(Tuple{T.parameters[1]...}, T.parameters[2],Val(s),0)
end

@inline function _fielddescr(::Type{syms}, ::Type{types},::Val{s},shift::Int) where {syms <: Tuple, types<:Tuple, s} 
    @inbounds begin
        syms===Tuple{} && throw(ArgumentError(s))
        type = Base.tuple_type_head(types)
        if s===Base.tuple_type_head(syms)
            return type, shift, bitsizeof(type)
        end
        _fielddescr(Base.tuple_type_tail(syms),Base.tuple_type_tail(types),Val(s),shift+bitsizeof(type))
    end
end


"""
    specialize(::Type{BitStruct{T}})

Specializes field access methods for BitStruct{T}, they are redefined as
optimized, inlined methods. It gives a dramatic performance boost 
(about factor 1000) for BitStruct field access methods.

Call it if runtime performance has any relevance for your application.
Call it once, immediately after BitStruct type creation.
    
: they compile down to a bare >> and & operation, even beating 
access speed of a mutable struct field access.
Call it if runtime performance has any relevance for your application.
Call it once, immediately after BitStruct type creation.

Precondition: all field types of BitStruct{T} must be already defined, including
methods encode, decode, bitsizeof for all field types used in BitStruct{T}.

Any reasons not to call it? Not really. Well, it compiles 3 methods 
per field of BitStruct{T}, causing some initialization overhead, 
and consuming some RAM. But these methods are very short, resource
consumption is neglible.

# How does it work? 

The basic idea of a BitStruct field is: it is 
a sequence of bits in an UInt64. Reading a BUInt{N} field of a BitStruct
is nothing more than a SHIFT operation >> and an AND operation & on the 
BitStruct binary value, an UInt64. They will almost always be applied to a 
CPU register variable, each consuming typically 1 CPU cycle, no memory access. 
With a call to specialize, the BitStruct field specific parameters for SHIFT and AND
are computed once in the specialize call, and used as constants in code generation.

Without this specialization, the bitmask parameters of a field are computed
at runtime, in every call, including a sequential search for the field name
in the field name list, bounds checks, maybe complex computation to determine 
the size in bits of a field.

Julias compiler is already very smart in what is called constant propagation, 
but has a priority on safety. It will not do optimizations if it cannot prove 
its preconditions. There are some "compiler switches" for more aggressive
optimization, like Base.@pure, but they are at risk. One of them is, that
a pure function must not be redefined: it has to return the same value for
the same set of parameters forever. But in julia, everyone can redefine every
(generic) function at any time, including BitStruct.bitsizeof which is at
core of bitfield parameter computation. 

Function specialize has a precondition: it requires that bitsizeof returns
the same result for a type used as field type in BitStruct{T}, 
during the time period from the specialize call, until
package BitStructs is recompiled. Given this condition, it calculates
bitfields parameters for all fields of BitStruct{T}, and generates methods
for each field, using computed bitfield parameters as constants.
In other words: it does constant propagation itself, with relaxed 
preconditions, compared to those julia compiler needs.
"""
function specialize(::Type{BitStruct{T}};getter::Bool=false,setter::Bool=false) where {T}
    shift = 0
    types = T.parameters[2].parameters
    syms = T.parameters[1]
    idx = 1
    btype = BitStruct{T}
    while idx <= length(syms)
        sym = syms[idx]
        type = types[idx]
        bits = bitsizeof(type)
        # now we know the bitfield parameters of field sym.
        # we generate _fielddescr method:
        #ex = :(function _fielddescr(::Type{$(esc(type))}, ::Val{$(esc(sym))}) 
        #return ($(esc(type)), $shift, $bits)
        strsym=string(sym)
        ex = :(function _fielddescr(::Type{$btype}, ::Val{Symbol($strsym)}) 
        return ($type, $shift, $bits)
        end)
        println("about to compile: ",ex)
        eval(ex) # this compiles the specialized function
        
        shift += bits
        idx += 1
    end
    return nothing
end

#=
"""
longtype(BitStruct{T})

return the full type definition in 1-line macro format for print/show/dump

"""
=#
function Base.string(::Type{BitStruct{T}}) where T<:NamedTuple
    types = Tuple(T.parameters[2].parameters)
    syms = T.parameters[1]
    io = IOBuffer()
    print(io,"BitStruct{")
    for i in 1:length(syms)
        s = syms[i]
        t = types[i]
        delim = i==length(syms) ? '}' : ','
        print(io,s, ":",t,delim)
    end
    return String(take!(io))
end


function Base.string(x::BitStruct{T}) where T<:NamedTuple
    types = Tuple(T.parameters[2].parameters)
    syms = T.parameters[1]
    io = IOBuffer()
    print(io,string(BitStruct{T}),'(')
    for i in 1:length(syms)
        delim = i==length(syms) ? ')' : ','
        print(io,getproperty(x,syms[i]),delim)
    end
    #close(io)
    return String(take!(io))
end

# elaborated version
function Base.show(io::IO, ::MIME"text/plain", x::BitStruct{T}) where T<:NamedTuple
    ps = reinterpret(UInt64,x)
    println(io, "BitStruct(",repr(ps),')')
    #types = Tuple(T.parameters[2].parameters)
    syms = T.parameters[1]
    for i in 1:length(syms)
        s = syms[i]
        #t = types[i]
        #println("  ",s, "::",t, " , ",shift," , ",bits)
        v = getproperty(x,s)
        println("  ",s, ":",typeof(v),' ',repr(getproperty(x,s)))
    end
    println("end")
end

# short 1-liner
function Base.show(io::IO, x::BitStruct{T}) where T<:NamedTuple
    print(io,string(x))
end


function Base.dump(io::IOContext, x::BitStruct{T}, n::Int, indent) where T <: NamedTuple
    ps = reinterpret(UInt64,x)
    types = Tuple(T.parameters[2].parameters)
    syms = T.parameters[1]
    println(io, "BitStruct(",repr(ps),')')
    if n >0 # no pointers in BitStruct ==> no check of circular structures necessary
        #recur_io = IOContext(io, Pair{Symbol,Any}(:SHOWN_SET, x))
        indent2 = string(indent,"  ")
        for s in syms
            #property
            #println("  ",s, "::",t, " , ",shift," , ",bits)
            t,shift, bits = _fielddescr(BitStruct{T},Val(s))
            v = getproperty(x,s)
            print(io,indent2,s," [",shift+1,':',shift+bits,"::",t, "]: ")
            Base.dump(io, v,n-1,indent2)
            println(io)
        end
    end
    nothing
end


"""
    set(ps::BitStruct{T};s::Symbol,v)

replace field s in ps by v.
"""
@inline function set(x::BitStruct{T},s::Symbol,v::V) where {T<:NamedTuple,V}
    ret = reinterpret(UInt64,x)
    t,shift, bits = _fielddescr(BitStruct{T},Val(s))
    u = encode(t,v)
    ret = _set(ret,Val(shift),Val(bits),u)
    return reinterpret(BitStruct{T},ret)
end

/=
# optimized code when copying a field from another BitStruct instance
@inline function set(x::BitStruct{T},s::Symbol,v::BitStruct{T}) where {T<:NamedTuple}
    t,shift, bits = _fielddescr(BitStruct{T},Val(s))
    mask = _mask(bits) << shift
    u = reinterpret(UInt64,x) & mask
    ret = (reinterpret(UInt64,x) & ~mask) | u
    return reinterpret(BitStruct{T},ret)
end
=/

"""
    set(ps::BitStruct{T};kwargs...)

replace a selection of fields given by named parameters.
parameter names in args must match properties of ps
"""
function setm(x::BitStruct{T};kwargs...) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    for p in pairs(kwargs)
        s = p.first
        t,shift, bits = _fielddescr(BitStruct{T},Val(s))
        v = encode(t,p.second)
        ret = _set(ret,Val(shift),Val(bits),v)
    end
    return reinterpret(BitStruct{T},ret)
end


# for benchmarking, only
function setm2(x::BitStruct{T};kwargs...) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    nt = kwargs.data # the named tupla
    syms = typeof(nt).parameters[1]
    idx = 1
    while idx <= length(syms)
        s = syms[idx]
        t,shift, bits = _fielddescr(BitStruct{T},Val(s))
        v = encode(t,nt[idx])
        ret = _set(ret,Val(shift),Val(bits),v)
    end
    return reinterpret(BitStruct{T},ret)
end


## overloaded operators


@inline function Base.getproperty(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescr(BitStruct{T},Val(s))
    return decode(type,_get(reinterpret(UInt64,x),shift,bits))
end


"""
    bs::BitStruct{T}   /   (fld,value)::Tuple{Symbol,Any}


fld must be a property of BitStruct{T} (a symbol contained in T, identifying a field).
The operation / replaces the bitfield fld in bs with value and returns the result.

In addition, the syntax ´bs /= fld,value´ is supported for a variable bs of 
type BitStruct{T}, it stores the result in bs.
"""
@inline function Base.:(/)(x::BitStruct{T},t::Tuple{Symbol,V}) where {T<:NamedTuple, V}
    set(x,t[1],t[2])
end


"""
    x::BitStruct{T}   +   fld::Symbol

fld must be a property of BitStruct{T} (a symbol contained in T, identifying a field).

Operation + sets all bits in bitfield fld to 1. Its primary use is to set a Bool
bitfield to true. For bitfield types other than Bool, please double check its use.
It has **nothing** in common with an arithmetic addition, it is more a set-like
union or inclusion. 

The following syntax variants are supported

´x += fld´ stores the result in x. Requires x is a variable of type BitStruct{T}.

´x + fld1 + fld2 + ... + fldn´ sets all bits in all specified fields fld1..fldn

**WARNING** a bitfield value with all bits set is probably undefined, causing 
severe errors on an attempt to decode such a bitfield value to its external 
representation.

Operation + is safe for bitfield types Bool, BUInt{N}, BInt{N}, 
and all primitive Integer types. It is **NOT ** safe for most Enum types, 
Float16, Float32 and probably user-defined bitfield types.
"""
@inline function Base.:(+)(x::BitStruct{T},fld::Symbol) where {T<:NamedTuple}
    type,shift,bits = _fielddescr(BitStruct{T},Val(fld))
    return reinterpret(BitStruct{T},reinterpret(UInt64,x)| (_mask(bits) << shift))
end


"""
    x::BitStruct{T}   -   fld::Symbol

fld must be a property of BitStruct{T} (a symbol contained in T, identifying a field).

Operation - sets all bits in bitfield fld to 0. Its primary use is to set a Bool
bitfield to false. Carefully check any use for non-Bool bitfields. It has 
**nothing** to do with a arithmetic sub traction, it is more like a set difference,
if you think of a BitStruct as a set of bits, in this interpretation
the symbol parameter is a subset containing all bis used by the bitfield.  

The following syntax variants are supported

´x -= fld´ stores the result in x. Requires x is a variable of type BitStruct{T}.

´x - fld1 - fld2 - ... - fldn´ clears all bits in all specified fields fld1..fldn

**WARNING** a bitfield value of 0 ( all bits unset) might be undefined, causing 
severe errors on an attempt to decode such a bitfield value to its external 
representation.

Operation & is safe for most bitfield types,in particulart Bool, BUInt{N}, BInt{N}, 
and all primitive Integer types, and Enums. It sets the bitfield value to 0,
for all predefined numeric bitfield types with an external type t it results
in zero(t), including Float16 and Float32. 

For enum types t, field access returns typemin(t).

For character types, its result is a control character with code 0, it might 
confuse C functions for strings, which rely on zero-terminated strings. 
"""
@inline function Base.:(-)(x::BitStruct{T},fld::Symbol) where {T<:NamedTuple}
    type,shift,bits = _fielddescr(BitStruct{T},Val(fld))
    return reinterpret(BitStruct{T},reinterpret(UInt64,x) & ~(_mask(bits) << shift))
end


"""
    x::BitStruct{T}   |   y::BitStruct{T}

Computes a BitStruct{T} as bitwise OR of x and y. Its primary use is to 
test if at least one flag bitfield of Type Bool is set. If x is theBitStruct 
to test, and y is given by ´y = BitStruct{T}() | fld1|fld2|...|fldn´, then
the expression ´x|y != BitStruct{T}()´ is true, if x has a bit set in any of
the bilfields fld1..fldn. 

**WARNING** a bitfield value with all bits set is probably undefined, causing 
severe errors on an attempt to decode such a bitfield value to its external 
representation.

Operation | is safe for bitfield types Bool, BUInt{N}, BInt{N}, 
and all primitive Integer types. It is **NOT ** safe for most Enum types, 
Float16, Float32 and probably user-defined bitfield types.
"""
@inline function Base.:(|)(x::BitStruct{T},y::BitStruct{T}) where {T<:NamedTuple}
    return reinterpret(BitStruct{T},reinterpret(UInt64,x) | reinterpret(UInt64,y))
end


"""
    x::BitStruct{T}   &   y::BitStruct{T}

Computes a BitStruct{T} as bitwise AND of x and y. Its primary use is to 
test if all values of a set of bitfields of Type Bool are true. If x is the BitStruct 
to test, and y is given by ´y = BitStruct{T}() | fld1|fld2|...|fldn´, then
the expression ´x&y ==y´ is true, iff x has all bits set in any of
the bitfields fld1..fldn. 

**WARNING** even if x and y are proper BitField values, x&y might contain 
undefined bitfields.

Operation & is safe for bitfield types Bool, BUInt{N}, BInt{N}, 
and all primitive Integer types. It is **NOT ** safe for most Enum types, 
Float16, Float32 and probably user-defined bitfield types.
"""
@inline function Base.:(&)(x::BitStruct{T},y::BitStruct{T}) where {T<:NamedTuple}
    return reinterpret(BitStruct{T},reinterpret(UInt64,x) & reinterpret(UInt64,y))
end



"""
    ~ x::BitStruct{T}

Inverts all bits of x which belong to bitfields (bits in the unterlying 
primitive 64 bit value which are not part of a bitfield are not changed). 
Its primary use is generation of masks for subsequent | and & operations. 

**WARNING** for a valid BitStruct x, ~x can have invalid bitfields.

Operation ~ is safe for bitfield types Bool, BUInt{N}, BInt{N}, 
and all primitive Integer types. It is **NOT ** safe for most Enum types, 
Float16, Float32 and probably user-defined bitfield types, which can have
invalid values in ~x for a valid x.
"""
@inline function Base.:(~)(x::BitStruct{T}) where {T<:NamedTuple}
    return reinterpret(BitStruct{T},(~reinterpret(UInt64,x)) & _mask(bitsizeof(BitStruct{T})))
    # todo
end


## constructors

#BitStruct{T}() where {T<:NamedTuple} = reinterpret(BitStruct{T},zero(UInt64))

# struct-alike constructor
function BitStruct{T}(args...) where T <:NamedTuple
    ret = reinterpret(BitStruct{T},zero(UInt64))
    syms = T.parameters[1]
    for (i,v) in enumerate(args)
        ret /= (syms[i],v)
    end
    return ret
end

# this is more specific than BitStruct{T}() ==> stack overflow. why??!
#function BitStruct{T}(;kwargs...) where {T<:NamedTuple}
#    set(reinterpret(BitStruct{T},zero(UInt64));kwargs...)
#end

# deprecated
#=
# first try: constructor setting some fields. TODO redesign using helper methods
"constructor setting some fields, fields not included in nt stay 0"
function BitStruct{T}(nt::NT) where {T<:NamedTuple, NT <: NamedTuple}
    ret = zero(UInt64)
    syms = NT.parameters[1]
    idx = 1
    while idx <= length(syms)
        s = syms[idx]
        t,shift, bits = _fielddescr(BitStruct{T},Val(s))
        local v::UInt64
        if t <: Union{BUInt,Unsigned} 
            v = UInt64(nt[idx])
            v >= (1<<bits) && error("overflow for type $t with $bits bits: value $v")
        end
        if t <: Union{BInt,Signed,Bool} 
            iv = Int64(nt[idx])
            (iv < -(1<<(bits-1)) || iv>= (1<<(bits-1)) ) && error("overflow for type $t with $bits bits: value $v")
            v = (iv%UInt64) & (1<<bits - 1)
        end
        if t <: Enum
            v = (Int(nt[idx])+typemin(t))%UInt64
            v >= (1<<bits) && error("overflow for type $t with $bits bits: value $v")
        end
        ret |= (v<<shift)
        idx += 1
    end
    return reinterpret(BitStruct{T},ret)
end
=#


"""
    @BitStruct name begin key1::Type1; key2::Type2; ...; end
 or @BitStruct name {key1::Type1, key2::Type2, ...}

 This macro defines a BitStruct and performance-optimized methods for BitStruct field access.
Syntax is that of [`@NamedTuple`](@ref), except the leading name

name is an identifier, it becomes a constant to be used as type alias 
for the constructed BitStruct type. 

It follows a block with a sequence of ident::type declarations,
ident is the field name in the BitStruct, and type its declared
type.

type must be an isbits data type. Parameterized data types are allowed. 
Included is support for all predefined primitive number types up to 32 bit
sizes, Bool (consuming 1 bit), Enum subtypes (bit size automatically derived),
and Char.

For BitStruct-s memory efficiency it is essential, that field definitions
can restrict the set of instances of a Type R to a small subset, which can 
be encoded with less bits. For this purpose, special singleton types
are introduced. Each such singleton type represents an encoding/decoding
scheme for an instance subset. You can define several types T1, T2 ...
identifying distinct subrange encodings of a type R.

For Integer subranges, the singleton types BInt{N} and BUInt{N} are defined in 
BitStructs package. BInt{N} declares an Int subrange of N bits, BUInt{n} an
UInt subrange. For Characters, the frequently used subranges of ASCII and Latin-1
characters are supported witn AsciiChar and LatinChar, consuming 7/8 bits in a
BitStruct.

It is easy to define support for further types and subranges of them
in BitStruct fields. The following methods have to be 
implemented for values of type R to be stored in a BitStruct bitfield
declared as type T: 
    
    * BitStructs.bitsizeof(::Type{T}). Number of bits to store a value of 
      type R in BitStruct field declared as type T.

    * BitStructs.encode(::Type{T},x::R).  Compute the bitfield 
      value representing x, to store in a BitStruct instance. Returned value
      must be a UInt64 in the range 0:(1<<bitsizeof(T)-1). 

    * BitStructs.decode(::Type{T}, x::UInt64)::R. Returns an instance of R
      constructed from a bitfield of bitsizeof(T) bits given in x.

All types must be already defined when @bitstruct is called. 

The sum of bit sizes of all fields must be smaller or equal to 64, because a BitStruct 
instance is implemented as a 64 bit primitive type.

Delimiter ';' between two field declarations can be replaced by a newline sequence, closely
resembling the usual struct syntax.

The following example just fits into 64 bit. An ordinary struct would consume 19 bytes, 
more than doubling memory consumption. Concerning its runtime performance, have a look at the
benchmarks supplied as test functions.

```jldoctest
julia> @bitstruct MyBitStruct begin
    flag1 :: Bool
    flag2 :: Bool
    flag3 :: Bool
    flag4 :: Bool
    ascii1 :: BUInt{7}
    ascii2 :: BUInt{7}
    id1 :: BUInt{9} # 0..511
    id2 :: BUInt{12} # 0..4095
    delta1 :: BInt{9} # -256..255
    delta1 :: BInt{9} # -256..255 
    flag5 :: Bool
    flag6 :: Bool
    flag7 :: Bool
    flag8 :: Bool
    status:: BUInt(3) # 0..7
end

```

"""
macro bitstruct(name,ex)
    Meta.isexpr(ex, :braces) || Meta.isexpr(ex, :block) ||
        throw(ArgumentError("@bitstruct expects name {...} or name begin...end"))
    
    decls = filter(e -> !(e isa LineNumberNode), ex.args)
    all(e -> Meta.isexpr(e, :(::)), decls) || throw(ArgumentError("@bitstruct must contain a sequence of name::type expressions, nothing else"))
    for e in decls 
        #println("ex item: ",e," :: ",typeof(e), " -> args",e.args)        
    end
    vars = [QuoteNode(e.args[1]) for e in decls]
    types = [esc(e.args[2]) for e in decls]
    bitstruct = :(BitStruct{NamedTuple{($(vars...),), Tuple{$(types...)}}})
    #println("type is: ",bitstruct) 
    # so far adopted from @NamedTuples. Now: build fielddescr table
    #= not necessary because _fielddescr is not optimized.
    # and not working: loop fails on a type declared after this macro but before macro call: eval(Symbol(t)) returns ERROR: UndefVarError: ProcStatus not defined
    fieldsyms = Symbol[]
    fielddscrs = Tuple{DataType,Int,Int}[] # type, shift, bits
    shift = 0
    for e in decls 
        #println("sym = ",e.args[1], "type = ", e.args[2], " typeof=",typeof(e.args[2]))
        t = e.args[2] # type
        println("t = ",t)
        t = eval(Symbol(t))
        println("eval(t) = ",dump(t))
        bits = bitsizeof(t)
        push!(fieldsyms,e.args[1])
        push!(fielddscrs,(t,shift,bits))
        shift += bits
        #println("shift=",shift)
        shift > 64 && throw(DomainError(e.args[1],"BitStruct too huge: would exceed a total bitsize of 64"))
    end
    =#
    # build the expressions to execute
    ret = quote
        const $(esc(name)) = $(bitstruct)
    end
    return ret
end


function bitsizeof(::Type{BitStruct{T}}) where T
    lastsym = last(T.parameters[1])
    type,shift,bits = _fielddescr(BitStruct{T},Val(lastsym))
    return shift+bits
end

"""
    check(::Type(BitStruct))
    
Verify that all declared fields have supported bitfield types.
Verify that total bit size does not exceed 64
"""
function check(::Type{BitStruct{T}}) where T
    T <: NamedTuple || throw(DomainError(T,"Type Parameter of BitStruct is not a NamedTuple"))
    syms = T.parameters[1]
    types = T.parameters[2].parameters
    shift = 0
    i = 0
    for s in syms
        i += 1
        shift += bitsizeof(types[i])
    end
    shift <= 64 || throw(DomainError(BitStruct{T},"Total bitsize is $shift - does not fit into 64 bit"))
end


# required by dump(aBitStructType)
function Base.datatype_fieldtypes(::Type{BitStruct{T}}) where T
    T.parameters[2].parameters
end

# DEBUG version
macro bs(name,ex)
    Meta.isexpr(ex, :braces) || Meta.isexpr(ex, :block) ||
        throw(ArgumentError("@bitstruct expects name {...} or name begin...end"))
    
    #q = Symbol("ProcStatus")
    #println(q)
    #println(eval(q))
    #println(dump(eval(q)))
    decls = filter(e -> !(e isa LineNumberNode), ex.args)
    all(e -> Meta.isexpr(e, :(::)), decls) || throw(ArgumentError("@bitstruct must contain a sequence of name::type expressions, nothing else"))
    for e in decls 
        #println("ex item: ",e," :: ",typeof(e), " -> args",e.args)        
    end
    vars = [QuoteNode(e.args[1]) for e in decls]
    types = [esc(e.args[2]) for e in decls]
    bitstruct = :(BitStruct{NamedTuple{($(vars...),), Tuple{$(types...)}}})
    println("type is: ",bitstruct) 
    # so far adopted from @NamedTuples. Now: build fielddescr table
    fieldsyms = Symbol[]
    fielddscrs = Tuple{DataType,Int,Int}[] # type, shift, bits
    shift = 0
    for e in decls 
        #println("sym = ",e.args[1], "type = ", e.args[2], " typeof=",typeof(e.args[2]))
        t = e.args[2] # type
        println("t = ",t)
        t = eval(Symbol(t))
        println("eval(t) = ",dump(t))
        bits = bitsizeof(t)
        push!(fieldsyms,e.args[1])
        push!(fielddscrs,(t,shift,bits))
        shift += bits
        #println("shift=",shift)
        shift > 64 && throw(DomainError(e.args[1],"BitStruct too huge: would exceed a total bitsize of 64"))
    end
    # build the expressions to execute
    ret = quote
        const $(esc(name)) = $(bitstruct)
    end
    return ret
end
export @bs


#=
macro bitstruct(name,ex)
    Meta.isexpr(ex, :braces) || Meta.isexpr(ex, :block) ||
        throw(ArgumentError("@bitstruct expects name {...} or name begin...end"))
    
    decls = filter(e -> !(e isa LineNumberNode), ex.args)
    all(e -> e isa Symbol || Meta.isexpr(e, :(::)), decls) ||
        throw(ArgumentError("@bitstruct must contain a sequence of name::type expressions, nothing else"))
    vars = [QuoteNode(e isa Symbol ? e : e.args[1]) for e in decls]
    types = [esc(e isa Symbol ? :Any : e.args[2]) for e in decls]
    bitstruct = :(BitStruct{NamedTuple{($(vars...),), Tuple{$(types...)}}})
    # so far adopted from @NamedTuples. Now: build fielddescr table
    fieldsyms = Symbol[]
    fielddscrs = Tuple{DataType,Int,Int}[] # type, shift, bits
    for e in decls 
        
    end
    # build the expressions to execute
    bitstruct = :(BitStruct{NamedTuple{($(vars...),), Tuple{$(types...)}}})
end
=#

#=
# definiert const und legt fkt mit Val typeparameter an.
macro m5(ex1::Symbol, ex2::Expr)
    fctsym = Symbol("f")
    ret = quote
        const $(esc(ex1)) = $(esc(ex2))
        function $(esc(fctsym))(::Val{$(esc(ex1))}) 
            $(esc(ex2))
        end
    end
    show(ret)
    println()
    return ret
end

=#

