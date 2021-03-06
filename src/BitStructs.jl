module BitStructs
#
#include("bittypes.jl")

#include "(bstruct.jl")

"""
BStruct ist the central type of package BitStructs.

A BStruct is a primitive 64-bit type which supports properties 
similar to a (immutable) julia struct type.

Type structure is defined by a NamedTuple; for convenience,
a macro @struct is supplied which comes syntactically close
to a struct declaration.

In contrast to a julia struct, its fields are aligned at 
bit boundaries, not byte boundaries in the BStruct container. 
The biggest memory gain is achieved for boolean values and Enum-s 
with a few instances. 

This package adds Integer subtypes BInt{N} 
and BUInt{N} with N bits, which have a value range of 0:1<<N-1 
respective -1<<(N-1):1<<(N-1)-1. These types are singleton types
for BStruct declarations of Int and UInt fields with restricted
value ranges.

Other types T can be used in BStruct provided that the following 
methods are implemented

 * reininterpret(UInt64,v:T) 

 * bitsizeof(T) # defaults to 8*sizeof(T))

 * _convert(::Type{UInt64}, bits, x::T)

 * _convert(::Type{T}, x::UInt64)
 
"""
primitive type BStruct{T<:NamedTuple} 64 end


"""
Type to be used in BStruct field declarations, only.

It declares an UInt field using N bits, with a value range 
0:1<<N-1. A read access to such a field returns an UInt.

When storing a value to the field in a BStruct constructor or by [`set`](@ref),
a value range check is performed as @boundscheck.
"""
struct BUInt{N} end

"""
Type to be used in BStruct field declarations, only.

It declares an Int field using N bits, with a value range 
-1<<(N-1):1<<(N-1)-1. A read access to such a field returns an Int.

When storing a value to the field in a BStruct constructor or by [`set`](@ref),
a value range check is performed as @boundscheck.
"""
struct BInt{N} end


export BStruct, BInt, BUInt, bitsizeof


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

Base.fieldnames(::Type{BStruct{T}}) where T = T.parameters[1]
Base.propertynames(ps::BStruct{T}) where T = fieldnames(BStruct{T})

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
This is guaranteed by _convert(UInt64,bits,...).
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



"""
_convert(::Type{type}, x::UInt64)

convert a bitfield x from a BStruct to a value of Type *type*.

This function is intentionally different from Base.convert, for two reasons:

  * type flags like BInt{N} are used as conversion target, but the returned value 
  is of a different type (Int). This breaks the usual contract of Base.convert.
  
  * a value to convert from is technically an UInt64, but semantically it is something packed into a bitfield from a BStruct,
  and _convert may apply transformations to use less bits. An important example is Enum: there is a Base.convert(Enum,v::Int),
  and it differs substantially from _convert(Enum,v::UInt) it the Enum has negative instances. 
"""
_convert(::Type{type},v::UInt64)            where type          = convert(type,v)   # generic default implementation
_convert(::Type{UInt64},v::UInt64)                              = v                 # to avoid ambiguity

# specific conversions
_convert(::Type{type},v::UInt64)            where type<:BUInt   = v
_convert(::Type{BInt{bits}},v::UInt64)      where bits          = (v%Int64)<<(64-bits)>>(64-bits)
_convert(::Type{type},v::UInt64)            where type<:Signed  = (v%Int64)<<(64-bitsizeof(type))>>(64-bitsizeof(type))
_convert(::Type{type},v::UInt64)            where type<:Enum    = type(v-typemin(type))

# conversions from external property type to bitfield

"""
_convert(::Type{UInt64}, bits, x::T)

This function basically inverts _convert(::Type{T},ps::UInt64).

Convert from a value of type T to a bitfield in a BStruct, with bounds check (does result fit in *bits* bits)
parameter bits is necessary with respect to BStruct fields declared as BUInt{N} or BInt{N} in an BStruct: T is an
ordinary UInt or Int in these cases, with bitsizeof(T)!=bits. 

_convert assures that returned value is in 0:_mask(bits).
"""
_convert(::Type{UInt64},bits, v::T)         where T             = _convert(UInt64,bits,convert(UInt64,v)) # default

@inline function _convert(::Type{UInt64},bits,v::T) where T<:Unsigned   
    @boundscheck v > _mask(bits) && throw(BoundsError(v))
    return v % UInt64
end

@inline function _convert(::Type{UInt64},bits,v::T) where T<:Signed     
    @boundscheck ( v < -1<<(bits-1) || v>= 1<<(bits-1) ) &&  throw(BoundsError(v))
    return (v % UInt64)&_mask(bits)
end

@inline function _convert(::Type{UInt64},bits,v::T) where T<:Enum     
    u = (Int(v)+Int(typemin(T)))%UInt64
    @boundscheck u > _mask(bits)  &&  throw(BoundsError(v))
    return u % UInt64
end
    

"""
    function _fielddescr(::Type{BStruct{T}},s::Symbol)

extract field descriptor (type,shift,bits) from type info for a symbol S.
If S is not found, (Nothing,0,0) is returned.

This function is slow, even with multiple dispatch. It is replaced at
compile time by @bstruct which generates specialized fast methods, 
returning a constant
"""

function _fielddescr(::Type{BStruct{T}},::Val{s}) where {T<:NamedTuple, s}
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

@inline function Base.getproperty(x::BStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescr(BStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end

#=

the fastest variant known so far without @generated or @macro

@inline function _fielddescr5(::Type{BStruct{T}},s::Symbol) where T <: NamedTuple
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

@inline function getpropertyV5(x::BStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescr5(PStruct{T},s)
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end
export getpropertyV5




Base.@pure function Base.getproperty(x::BStruct{T},s::Symbol) where T<:NamedTuple
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
@inline Base.@pure function getpropertyV2(x::BStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescr(BStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end
export getpropertyV2

=#/


# first try: constructor setting some fields. TODO redesign using helper methods
"constructor setting some fields, fields not included in nt stay 0"
function BStruct{T}(nt::NT) where {T<:NamedTuple, NT <: NamedTuple}
    ret = zero(UInt64)
    syms = NT.parameters[1]
    idx = 1
    while idx <= length(syms)
        s = syms[idx]
        t,shift, bits = _fielddescr(BStruct{T},Val(s))
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
    return reinterpret(BStruct{T},ret)
end


function Base.show(x::BStruct{T}) where T<:NamedTuple
    ps = reinterpret(UInt64,x)
    println(BStruct{T}, ' ',repr(ps))
    types = Tuple(T.parameters[2].parameters)
    syms = T.parameters[1]
    for s in syms
        t,shift, bits = _fielddescr(BStruct{T},Val(s))
        println("  ",s, "::",t, " = ",repr(_convert(t,_get(ps,shift,bits))))
    end
    println("end")
end


"""
    set(ps::BStruct{T};kwargs...)

replace a selection of fields given by named parameters.
parameter names in args must match properties of ps, 
and there must be a method _convert(UInt64,bits,v) for any value v in kwargs.
"""
function set(x::BStruct{T};kwargs...) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    for p in kwargs
        s = p.first
        t,shift, bits = _fielddescr(BStruct{T},Val(s))
        v = _convert(UInt64,bits,p.second)
        ret = _set(Val(shift),Val(bits),ret,v)
        ret |= (v<<shift)
    end
    return reinterpret(BStruct{T},ret)
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
Base.@pure function _fielddescrV3(::Type{BStruct{T}},::Val{s}) where {T<:NamedTuple,s} # s isa Symbol
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


@inline Base.@pure function getpropertyV3(x::BStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescrV3(BStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end
export getpropertyV3




Base.@pure function _descrkernel(::Type{T},s::Symbol) where T <: NTuple{N,Datatype}
    if N>0
        if s == ? hasfield


            function fieldindex(T::DataType, name::Symbol, err::Bool=true)
                return Int(ccall(:jl_field_index, Cint, (Any, Any, Cint), T, name, err)+1)
            end

Base.@pure function _fielddescrV2(::Type{BStruct{T}},::Val{s}) where {T<:NamedTuple,s} # s isa Symbol
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
@inline Base.@pure function getpropertyV3(x::BStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescrV3(BStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end
=#




end # module