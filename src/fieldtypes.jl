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
@inline function decode(::Type{T},v::UInt64) where T
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
    T(v%Int - Int(typemin(T)))
end
@inline function decode(::Type{T},v::UInt64) where T<:Union{AsciiChar,Latin1Char}
    @boundscheck checkbitsize(v,bitsizeof(T))
    Char(v)
end


# conversions from external property type to bitfield

"""
encode(v::T)

Convert a value of type T to a bitfield with bitsizeof(T) bits.
Bitfield is technically an UInt64 with a restricted range 0:(1<<bits)-1.

encode and decode are mutually inverse functions for valid arguments.
for a v::T with a supported type T must hold: decode(T,encode(v))==v

For a bitfield u which is convertable to an instance of T must hold:
encode(decode(T,u)) == u
"""
@inline encode(v::T) where T = encode(v,bitsizeof(T)) 

@inline function encode(v::T,bits) where T<:Unsigned   
    @boundscheck checkbitsize(v,bits)
    return v % UInt64
end

@inline function encode(v::T,bits) where T<:Signed     
    @boundscheck ( v < -1<<(bits-1) || v>= 1<<(bits-1) ) &&  throw(DomainError(v,"out of range for BInt{$bits}"))
    return (v % UInt64)&_mask(bits)
end


@inline function encode(v::T,bits) where T<:Enum     
    u = (Int(v)+Int(typemin(T)))%UInt64
    @boundscheck checkbitsize(u,bits)
    return u
end
    
@inline function encode(v::Bool,bits)
    return v%UInt64
end

@inline function encode(v::Float16,bits)
    return reinterpret(UInt16,v)%UInt64
end

@inline function encode(v::Float32,bits)
    return reinterpret(UInt32,v)%UInt64
end
   
@inline function encode(v::Char,bits)
    u = UInt64(v)
    @boundscheck checkbitsize(u,bits)
    return u
end
   
