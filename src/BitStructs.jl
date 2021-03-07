module BitStructs
#
#include("bittypes.jl")

#include "(BitStruct.jl")

"""
BitStruct ist the central type of package BitStructs.

A BitStruct is a primitive 64-bit type which supports properties 
similar to a (immutable) julia struct type.

In contrast to a julia struct, its fields are aligned at 
bit boundaries in the BitStruct container, which reduces
memory consumption for suitable field types at a very small price 
(a >>> and a & on an UInt64 value). The biggest memory gain is 
achieved for Bool fields (factor 8), very boolean values and Enum-s
with a few instances. 

BitStruct subtypes should always be defined using macro
[`@bitstruct`](@ref), which comes syntactically close
to a usual julia struct declaration. The macro generates
performance-optimized methods for field access, a BitStruct
subtype is not usable without that. 
"""
primitive type BitStruct{T<:NamedTuple} 64 end


"""
Type to be used in BitStruct field declarations, only.

It declares an UInt field using N bits, with a value range 
0:1<<N-1. A read access to such a field returns an UInt.

When storing a value to the field in a BitStruct constructor or by [`set`](@ref),
a value range check is performed as @boundscheck.
"""
struct BUInt{N} end

"""
Type to be used in BitStruct field declarations, only.

It declares an Int field using N bits, with a value range 
-1<<(N-1):1<<(N-1)-1. A read access to such a field returns an Int.

When storing a value to the field in a BitStruct constructor or by [`set`](@ref),
a value range check is performed as @boundscheck.
"""
struct BInt{N} end


export BitStruct, BInt, BUInt, bitsizeof


"""
    bitsizeof(T)

return the size of an instance of T in bits
"""
function bitsizeof end

bitsizeof(::Type{T}) where T = sizeof(T)*8
bitsizeof(::Type{BInt{N}}) where N = N
bitsizeof(::Type{BUInt{N}}) where N = N
bitsizeof(::Type{Bool}) = 1
@Base.pure bitsizeof(::Type{T}) where T<: Enum = 8*sizeof(Int) - leading_zeros(Int(typemax(T))-Int(typemin(T)))

Base.fieldnames(::Type{BitStruct{T}}) where T = T.parameters[1]
Base.propertynames(ps::BitStruct{T}) where T = fieldnames(BitStruct{T})

# NTuple{N, Any} is supertype of all tuples of length NamedTuple!!
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

Boundscheck tests if only the lowest *bits* bits are set in value. 
This is guaranteed by encode,bits,...).
"""
@inline function _set(pstruct::UInt64,::Val{shift},::Val{bits}, value::UInt64) where {shift, bits}
    v = value & _mask(bits)
    @boundscheck v==value || throw(BoundsError()) # error("Out of bitfield range: $value. shift=$shift bits=$bits")
    pstruct &= !(_mask(bits) << shift) # delete bitfield
    pstruct |= v << shift
    return pstruct
end

@Base.pure function _set(pstruct::UInt64,shift,bits,value::UInt64) 
    v = value & _mask(bits)
    @boundscheck v==value || throw(BoundsError()) # error("Out of bitfield range: $value. shift=$shift bits=$bits")
    pstruct &= !(_mask(bits) << shift) # delete bitfield
    pstruct |= v << shift
    return pstruct
end

function checkbitsize(v::UInt64,bits) 
    v< 1<<bits || throw("value has more than $bits bits: $v"outside allowed range of $bits" ArgumentError(string(v))")
end

"""
decode(::Type{T}, x::UInt64)

convert a bitfield x from a BitStruct to a value of Type T.

This function is intentionally different from Base.convert, for several reasons:

  * type flags like BInt{N} are used as conversion target, but the returned value 
  is of a different type (Int). This breaks the usual contract of Base.convert.
  
  * the value to convert from is declared as an UInt64, but in BitStruct semantics it is 
    a BUInt{bitsizeof(T)}, a bitfield from a BitStruct, with bitsizeof(T) significant bits.
  
  * there may be a Base.convert method with the same parameters doing something different.
  Example: Base.convert(Float16,UInt64(2)) will return Float16(2.0), decode(Float16,UInt64(2)) will return
  reinterpret(Float16,UInt16(2)).  
  
Important precondition, implemented as @boundscheck: v < 1<<bitsizeof(T). 
"""
@inline function decode(::Type{T},v::UInt64) where T
    @boundscheck checkbitsize(v,bitsizeof(T))
    convert(T,v)   # generic default implementation
end

# specific conversions
@inline function decode(::Type{T},v::UInt64) where T<:BUInt
    @boundscheck checkbitsize(v,bitsizeof(T))
    v
end
@inline function decode(::Type{T},v::UInt64) where T<:Unsigned 
    @boundscheck checkbitsize(v,bitsizeof(T))
    v % T
end
@inline function decode(::Type{BInt{bits}},v::UInt64) where bits
    @boundscheck checkbitsize(v,bits)
    (v%Int64)<<(64-bits)>>(64-bits) # sign extension
end
@inline function decode(::Type{T},v::UInt64) where T<:Signed
    @boundscheck checkbitsize(v,bitsizeof(T))
    (v%Int64)<<(64-bitsizeof(T))>>(64-bitsizeof(T))
end
@inline function decode(::Type{T},v::UInt64) where T<:Enum
    @boundscheck checkbitsize(v,bitsizeof(T))
    T(v-typemin(T))
end


# conversions from external property type to bitfield

"""
encode(v::T)

Convert a value of type T to a bitfield with bitsizeof(T) bits.
Bitfield is technically an UInt64 with a restricted range 0:(1<<bits)-1.
"""
@inline encode(v::T) where T<:Union{Signed,Unsigned} = encode(bitsizeof(T),v) 

@inline function encode(bits,v::T) where T<:Unsigned   
    @boundscheck checkbitsize(v,bits)
    return v % UInt64
end

@inline function encode(bits,v::T) where T<:Signed     
    @boundscheck ( v < -1<<(bits-1) || v>= 1<<(bits-1) ) &&  throw(BoundsError(v))
    return (v % UInt64)&_mask(bits)
end


@inline function encode(v::T) where T<:Enum     
    u = (Int(v)+Int(typemin(T)))%UInt64
    @boundscheck checkbitsize(u,bitssizeof(T))
    return u % UInt64
end
    
@inline function encode(v::Bool)
    return v%UInt64
end
    

"""
    function _fielddescr(::Type{BitStruct{T}},s::Symbol)

extract field descriptor (type,shift,bits) from type info for a symbol S.
If S is not found, (Nothing,0,0) is returned.

This function is slow, even with multiple dispatch. It is replaced at
compile time by @BitStruct which generates specialized fast methods, 
returning a constant
"""

function _fielddescr(::Type{BitStruct{T}},::Val{s}) where {T<:NamedTuple, s}
    shift = 0
    types = T.parameters[2].parameters
    syms = T.parameters[1]
    idx = 1
    while idx <= length(syms)
        type = types[idx]
        bits = bitsizeof(type)
        if syms[idx]===s
            return type,shift,bits
        end
        shift += bits
        idx += 1
    end
    throw(ArgumentError(s))
end

@inline function Base.getproperty(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescr(BitStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end

#=

the fastest variant known so far without @generated or @macro

@inline function _fielddescr5(::Type{BitStruct{T}},s::Symbol) where T <: NamedTuple
    _fielddescr5(Tuple{T.parameters[1]...}, T.parameters[2],s,0)
end

@inline function _fielddescr5(::Type{syms}, ::Type{types},s::Symbol,shift::Int) where {syms <: Tuple, types<:Tuple} 
    syms===Tuple{} && throw(ArgumentError(s))
    type = Base.tuple_type_head(types)
    if s===Base.tuple_type_head(syms)
        return type, shift, bitsizeof(type)
    end
    _fielddescr5(Base.tuple_type_tail(syms),Base.tuple_type_tail(types),s,shift+bitsizeof(type))
end

@inline function getpropertyV5(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescr5(PStruct{T},s)
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end
export getpropertyV5




Base.@pure function Base.getproperty(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    @inbounds begin
        shift = 0
        types = T.parameters[2].parameters
        syms = T.parameters[1]
        idx = 1
        while idx <= length(syms)
            type = types[idx] 
            bits = bitsizeof(type)
            if syms[idx]===s
                v = _get(reinterpret(UInt64,x),Val(shift),Val(bits))
                return _convert(type,v)
            end
            shift += bits
            idx += 1
        end
        throw(ArgumentError(s))
    end
end


# better than getproperty but still slow
@inline Base.@pure function getpropertyV2(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescr(BitStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end
export getpropertyV2

=#/


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


function Base.show(x::BitStruct{T}) where T<:NamedTuple
    ps = reinterpret(UInt64,x)
    println(BitStruct{T}, ' ',repr(ps))
    types = Tuple(T.parameters[2].parameters)
    syms = T.parameters[1]
    for s in syms
        t,shift, bits = _fielddescr(BitStruct{T},Val(s))
        println("  ",s, "::",t, " = ",repr(_convert(t,_get(ps,shift,bits))))
    end
    println("end")
end


"""
    set(ps::BitStruct{T};kwargs...)

replace a selection of fields given by named parameters.
parameter names in args must match properties of ps
"""
function set(x::BitStruct{T};kwargs...) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    for p in kwargs
        s = p.first
        t,shift, bits = _fielddescr(BitStruct{T},Val(s))
        v = encode(p.second)
        ret = _set(Val(shift),Val(bits),ret,v)
        ret |= (v<<shift)
    end
    return reinterpret(BitStruct{T},ret)
end



# DEPRECATED use _convert . But: seems like compiler does optimize this 
@inline @Base.pure function Base.convert(::Type{type},::Val{shift},::Val{bits},x::UInt64) where {type,shift,bits}
    v = (x >>> shift) & (one(UInt64)<<bits - one(UInt64))
    type <: BUInt && return v
    type <: BInt && return (v%Int64)<<(64-bits)>>(64-bits)
    type <: Signed && return type((v%Int64)<<(64-bits)>>(64-bits))
    type <: Enum && return type(v-typemin(type))
    return type(v)
end


#= WIP - plz ignore
Base.@pure function _fielddescrV3(::Type{BitStruct{T}},::Val{s}) where {T<:NamedTuple,s} # s isa Symbol
    shift = 0
    types = T.parameters[2].parameters
    syms = T.parameters[1]
    idx = = 
    while idx <= length(syms)
        type :: DataType = types[idx] # type annotation should be unnecessary - compiler knows structure of T
        bits = bitsizeof(type)
        if syms[idx]===s
            return type,shift, bits
        end
        shift += bits
        idx += 1
    end
    # symbol not found - clearly an error. what to do to keep method pure and type-stable?
    throw(ArgumentError(s))
    #variant 1: type stable default answer - needs further treatment in caller.
    return Nothing,0,0 # is dead code if compiler recognizes throw as some form of return
    #variant 2: throw an exception. Is that type stable?!!
    #throw(ErrorException("symbol $S not found in $T"))
end


@inline Base.@pure function getpropertyV3(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescrV3(BitStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end
export getpropertyV3




Base.@pure function _descrkernel(::Type{T},s::Symbol) where T <: NTuple{N,Datatype}
    if N>0
        if s == ? hasfield


            function fieldindex(T::DataType, name::Symbol, err::Bool=true)
                return Int(ccall(:jl_field_index, Cint, (Any, Any, Cint), T, name, err)+1)
            end

Base.@pure function _fielddescrV2(::Type{BitStruct{T}},::Val{s}) where {T<:NamedTuple,s} # s isa Symbol
    shift = 0
    types = Tuple(T.parameters[2].parameters)
    syms = T.parameters[1]
    idx = 1
    while idx <= length(syms)
        type :: DataType = types[idx] # type annotation should be unnecessary - compiler knows structure of T
        bits = bitsizeof(type)
        if syms[idx]===s
            return type,shift, bits
        end
        shift += bits
        idx += 1
    end
    # symbol not found - clearly an error. what to do to keep method pure and type-stable?
    throw(ArgumentError(s))
    #variant 1: type stable default answer - needs further treatment in caller.
    return Nothing,0,0 # is dead code if compiler recognizes throw as some form of return
    #variant 2: throw an exception. Is that type stable?!!
    #throw(ErrorException("symbol $S not found in $T"))
end


# compiler does not 
@inline Base.@pure function getpropertyV3(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescrV3(BitStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end
=#

"""
    @BitStruct name begin key1::Type1; key2::Type2; ...; end
 or @BitStruct name {key1::Type1, key2::Type2, ...}

 This macro defines a BitStruct and performance-optimized methods for BitStruct field access.
Syntax is that of [`@NamedTuple`](@ref), except the leading name

name is an identifier, it is a type alias for the constructed BitStruct type. 

Its first ex follows a sequence of identifier :: type declarations,
identifier becomes a field name in the BitStruct, and type its type.

type must be an isbits data type. Parameterized data types are allowed. 
Included is support for all predefined primitive number types up to 32 bit
sizes, Bool (consuming 1 bit) and Enum subtypes (bit size automatically derived).

For Integer subranges, the types BInt{N} and BUInt{N} are defined in BitStructs 
package, they are used as type markers within BitStruct definitions, only.
Technically, they are singleton typesmarker types which declare 
a field as Int or UInt with a subrange consuming N bits in the BitStruct instance.
A field declared as BInt{N} / BUInt{N]} is read and written as Int/UInt, 
with a range restriction to N significant bits.

Other types are accepted, but need some support: the following methods have to be 
implemented for a type T to be supported in a BitStruct:
    
    * BitStructs.bitsizeof(T). Number of bits to store a value of type T in
      a BitStruct instance. If no specific method is defined, 8*sizeof(T)
      is used as a default - probably a waste of bits.

    * BitStructs.encode(x::T) returning the bitfield 
      value to store in a BitStruct instance representing x. returned value
      must be a UInt64 in the range 0:(1<<bitsizeof(T)-1). 
      If no specific method is defined, Base.convert is called with the same parameters.

    * BitStructs.decode(::Type{T}, x::UInt64) returning an instance of T
      constructed from a bitfield of bitsizeof(T) bits given in x.

All types and their bitsizeof method must be defined if @BitStruct is called.

The sum of bit sizes of all fields must be smaller or equal to 64, because a BitStruct 
instance is implemented as a 64 bit primitive type (is checked by the macro).

Delimiter ';' between two field declarations can be replaced by a newline sequence, closely
resembling the usual struct syntax.

The following example just fits into 64 bit. An ordinary struct would consume 19 bytes, 
more than doubling memory consumption. Concerning its runtime performance, have a look at the
benchmarks supplied as test functions.

```jldoctest
julia> @BitStruct MyBitStruct begin
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
    all(e -> e isa Symbol || Meta.isexpr(e, :(::)), decls) ||
        throw(ArgumentError("@NamedTuple must contain a sequence of name::type expressions"))
    vars = [QuoteNode(e isa Symbol ? e : e.args[1]) for e in decls]
    types = [esc(e isa Symbol ? :Any : e.args[2]) for e in decls]
    # build the expressions to execute
    bitstruct = :(BitStruct{NamedTuple{($(vars...),), Tuple{$(types...)}}})
end




end # module