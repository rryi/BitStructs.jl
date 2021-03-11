# this file contains type definitions for fields in a BitStruct and necessary helper methods for types supported out-of-the-box


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


"""
Type to be used in BitStruct field declarations, only.

It declares a Char field with restricted value range 
for ASCII characters, only.

Encoding in a BitStruct consumes 7 bits.

A read access to such a field returns a Char representing an
ASCII character. , meaning Int(). Int.

When storing a Char value to such a field, 
a value range check is performed as @boundscheck.
for a c::Char, range check verifies UInt(c)<=127.
"""
struct AsciiChar end

"""
Type to be used in BitStruct field declarations, only.

It declares a Char field with restricted value range 
covering all characters defined in the Latin1 encoding, which is 
the same as ISO-8859-1.

In Unicode, these characters map on ihe first 256 code points.

Encoding in a BitStruct consumes 8 bits.

A read access to such a field returns a Char representing an
Latin1 character.

When storing a Char value to such a field, 
a value range check is performed as @boundscheck.
for a c::Char, range check verifies UInt(c)<=255
"""
struct Latin1Char end


"""
    bitsizeof(T)

return the number of bits which are sufficient to encode an instance of T
as a bitfield. size of an instance of T in bits
"""
function bitsizeof end

# bitsizeof(::Type{T}) where T = sizeof(T)*8
bitsizeof(::Type{BInt{N}}) where N = N
bitsizeof(::Type{BUInt{N}}) where N = N
bitsizeof(::Type{Bool}) = 1
bitsizeof(::Type{T}) where T<: Enum = 8*sizeof(Int) - leading_zeros(Int(typemax(T))-Int(typemin(T)))

bitsizeof(::Type{T}) where T<: Union{UInt8,UInt16,UInt32,Int8,Int16,Int32,Float16,Float32,Char} = 8*sizeof(T)
bitsizeof(::Type{AsciiChar}) = 7
bitsizeof(::Type{Latin1Char}) = 8



"throw an error if v used more that its lowest *bits* bits"
function checkbitsize(v::UInt64,bits) 
    (v< 1<<bits) || throw(ArgumentError("value has more than $bits bits: $v"))
end


# conversion from a bitfield to external type


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
@inline function decode(::Type{T},v::UInt64) where T <: Bool
    @boundscheck checkbitsize(v,bitsizeof(T))
    convert(T,v)   # generic default implementation
end

# specific conversions
@inline function decode(::Type{BUInt{bits}},v::UInt64) where bits
    @boundscheck checkbitsize(v,bits)
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
    T((v%Int64)<<(64-bitsizeof(T))>>(64-bitsizeof(T)))
end
@inline function decode(::Type{T},v::UInt64) where T<:Enum
    @boundscheck checkbitsize(v,bitsizeof(T))
    T(v%Int + Int(typemin(T)))
end
@inline function decode(::Type{T},v::UInt64) where T<:Union{AsciiChar,Latin1Char}
    @boundscheck checkbitsize(v,bitsizeof(T))
    Char(v)
end


# conversions from external property type to bitfield

"""
encode(::Type{T}, v::V)

Convert a value of type V to a bitfield value for a BitStruct
field declared as type T. A bitfield in the BitStruct context
is technically an UInt64 with a value below 1<<bitsizeof(T).

encode and decode are in some sense inverse functions. Lets look
at a BitStruct field declared as T. With encode(T,v), we compute the
BitStruct-internal representation as a bitfield of bitsizeof(T) bits.
With decode(T,u::UInt) we reconstruct its representation for
processing outside of BitStruct. Its data type is defined by the
return value of decode, let-s call it R.

In simple cases like Bool we have T===R===V. 

If T is a singleton type, only used to define a field of a
BitStruct, like BInt{N} or AsciiChar, R and V always differ from T,
but in most cases, we have still R===V.

To allow for type-relaxed assignments to BitStruct fields, you
can define encode on several "similar" types, we arrive at R<:V.

encode and decode are mutually inverse functions for valid arguments.
for v=Decode(T,u) the following must hold: *encode(T,v)==u* and
also *decode(T,encode(T,v)==v*. 
For other values v, the relation *decode(T,encode(T,v))==v* is not
granted. This is the case if encode accepts arguments of "similar"
types, or encode already does a projection, like in the case of type
Sign in tutorial.jl. For similar types with suitable convert and 
promote rules the relation usually holds.
"""
@inline function encode(::Type{T}, v::V) where {T<:Union{Unsigned,BUInt}, V<:Unsigned}
    u = v%UInt64
    @boundscheck checkbitsize(u,bitsizeof(T))
    return u
end

@inline function encode(::Type{T}, v::V) where {T<:Union{Signed,BInt}, V<:Integer}
    bits = bitsizeof(T)
    @boundscheck ( v < -1<<(bits-1) || v>= 1<<(bits-1) ) &&  throw(DomainError(v,"out of range for bitfield $T"))
    return (v % UInt64)&_mask(bits)
end


@inline function encode(::Type{T}, v::T) where {T<:Enum}     
    u = (Int(v)-Int(typemin(T)))%UInt64
    @boundscheck checkbitsize(u,bitsizeof(T))
    return u
end
    
@inline function encode(::Type{T}, v::T) where {T<:Bool}   
    return v%UInt64
end

@inline function encode(::Type{T}, v::V) where {T<:Float16, V<:Real}
    return reinterpret(UInt16,T(v))%UInt64
end

@inline function encode(::Type{T}, v::V) where {T<:Float32, V<:Real}
    return reinterpret(UInt32,T(v))%UInt64
end

   
@inline function encode(::Type{T}, v::V) where {T<:Union{Char,AsciiChar,Latin1Char}, V<:Char}
    u = UInt64(v)
    @boundscheck checkbitsize(u,bitsizeof(T))
    return u
end
   
