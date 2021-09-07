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



"""
_mask(Int::bits) :: UInt64

return a bit mask to restrict to the lowest *bits* bits of an UInt64.
"""
@inline _mask(bits) = one(UInt64)<<bits - one(UInt64)


"""
    _get(bitstruct::UInt64, shift, bits)

extract a bitfield from a packed struct.
If bitstruct is interpreted as a bit vector, it returns bitstruct[shift+1:shift+bits] 
"""
@inline _get(bitstruct::UInt64, shift, bits) = (bitstruct>>>shift) & _mask(bits)

# this variant was not faster inm benchmarking.
#@inline _get(bitstruct::UInt64, ::Val{shift},::Val{bits}) where {shift,bits} = (bitstruct>>>shift) & _mask(bits)



"""
    _set(bitstruct::UInt64, shift, bits, value::UInt64)

set a bitfield in a packed struct.
If bitstruct and value are interpreted as bit vector, it performs bitstruct[shift+1:shift+bits] = value[1..bits]

Boundscheck tests if no bit is set in balue except the lowest *bits* bits. 
This is guaranteed by encode(...,bits).
"""
@inline function _set(bitstruct::UInt64,shift,bits,value::UInt64) 
    @boundscheck checkbitsize(value,bits)
    bitstruct &= ~(_mask(bits) << shift) # delete bitfield
    bitstruct |= value << shift
    return bitstruct
end

# variant for benchmarking... was not faster in setperoperty with constant bits,shift parameters
# !!! but had strange effect: return value was Any instead of UInt64 !!!
#=
@inline function _set(bitstruct::UInt64,::Val{shift},::Val{bits}, value::UInt64) where {shift, bits}
    @boundscheck checkbitsize(value,bits)
    bitstruct &= ~(_mask(bits) << shift) # delete bitfield
    bitstruct |= value << shift
    return bitstruct
end
=#


"""
    function _fielddescr(::Type{BitStruct{T}},s::Symbol)

extract field descriptor tuple (type,shift,bits,index,externalType) 
from BitStruct type parameters for a symbol S. 
If s is not found, an ArgumentError is thrown. Explanation:

 - type the bitfield type given as type in t
 - shift no. of bits to shift right to move lsb of bitfield to lsb
   (lsb===least significant bit)
 - bits number of bits of the bitfield 
 - index index of s in the symbol tuple of t
 - externalType the data type returned by *decode(type)*,
   also called "external type" of a bitfield in the BitStructs doc.

This function would be an ideal candidate for @generated, 
if it would not suffer from "new method in old world" problem: 
any type defined after its first use will cause an exception 
if the type is used in a BitStruct. 

Solution: a call of [`generate`](@ref) compiles it down to return
constants as inlined code. This also removes unused return parameters
in native code.
"""
function _fielddescr end

# generic version. Specialized methods are created by generate(..)
function _fielddescr(::Type{BitStruct{T}},s::Symbol) where {T<:NamedTuple} 
    #println("generic _fielddescr s=$s T=$T")
    shift = 0
    types = T.parameters[2].parameters
    syms = T.parameters[1]
    idx = 1
    while idx <= length(syms)
        type = types[idx]
        #bits = Int(Base.invokelatest(bitsizeof,type)) # bitsizeof must be recursively defined with invokelatest!!!
        bits = bitsizeof(type)
        R = typeof(decode(type, zero(UInt64)))
        if syms[idx]===s
            # @generated: return :(($type,$shift,$bits))
            return type,shift,bits,idx,R
        end
        shift += bits
        idx += 1
    end
    throw(ArgumentError(string(s))) 
end



Base.fieldnames(::Type{BitStruct{T}}) where T = T.parameters[1]

Base.propertynames(ps::BitStruct{T}) where T = fieldnames(BitStruct{T})

# fieldname
Base.fieldname(::Type{BitStruct{T}},i) where T = T.parameters[1][i]


"""
fieldtypes(BitStruct{T}) :: NTuple(N,Datatype)

Return a tuple with the external types of a BitStruct - the
types of the values returned by getproperty and expected by setproperty. 
"""
function Base.fieldtypes(::Type{BitStruct{T}}) where {T<:NamedTuple} 
    v = DataType[]
    syms = T.parameters[1]
    for s in syms
        t,shift, bits, idx, R = _fielddescr(BitStruct{T},s)
        push!(v,R)
    end
    return tuple(v...)
end


"""
bitfieldtypes(BitStruct{T}) :: NTuple(N,Datatype)

Return a tuple with the bitfield declarative types of a BitStruct - the
types used in the NamedTuple which defines a BitStruct. 
"""
bitfieldtypes(::Type{BitStruct{T}}) where {T<:NamedTuple} = tuple(T.parameters[2].parameters...)



# recursive implementation - was for subfield direct access which turned out to have no advantage
#=
function _generate(targetType::DataType, bsType::DataType, shift::Int, prefix::String, subfields::Bool) 
    types = bsType.parameters[1].parameters[2].parameters
    syms = bsType.parameters[1].parameters[1]
    idx = 1
    while idx <= length(syms)
        sym = syms[idx]
        type = types[idx]
        bits = bitsizeof(type)
        strsym=prefix*string(sym)
        ex = :(@inline function _fielddescr(::Type{$targetType}, ::Val{Symbol($strsym)}) 
        return ($type, $shift, $bits)
        end)
        #println("about to compile: ",ex)
        eval(ex) # this compiles the specialized entry function
        if subfields && type <: BitStruct
            # generate also all fields of type as fields of target, with sym * '_' as prefix
            _generate(targetType,type,shift,strsym*"_",subfields)
        end
        shift += bits
        idx += 1
    end
    return nothing
end
=#


# TODO maybe we should split into 3 functions, returning 1 value, each?!

"""
    generate(m::Module)    
    generate(bitStruct::Datatype)


*generate* compiles really fast methods for field access of BitStruct types. 

# when and how to use

Calling *generate* is not mandatory, but highly recommended. Most BitStruct
operations are about factor 1000 (!!!) faster after *generate* was called.

*generate* has the following preconditions
for a call with a bitStruct type as parameter:
    
 * *generate* must be called at top level (not inside a function or block)
 * any type T used as bitfield type inside of type bitStruct must alreeady exist
 * [`bitsizeof`](@ref)(T) for such a type T must be defined

As a rule of thumb, define all BitStruct types and their custom bitfield types
at the beginning of a logical source code unit (a module, or a .jl file included at
top level), including all bitszizeof method definitions, and then call *generate*.

You should call *generate* before BitStruct instances are created. You must define
*encode* and *decode* methods for custom bitfield types before you access bitfields
of that type.

Calling *generate* with a module as parameter, will generate for all BitStruct types
found in that module at the time of the call.

# why does *generate* speed up BitStruct field access by factor 1000?

A bitfield read access is not more than a SHIFT and an AND operation on a 64-bit value - 
if the value for SHIFT and the bitmask for AND are already known. They depend on the 
bitfield name, calculating them requires a loop over BitStruct field names which needed 
about factor 1000 more CPU time than the SHIFT and AND operation, in our benchmarks. 

*generate* replaces the iterative default implementation for bitfield access by generated code,
which is specific for every field of a BitStruct, with constants for SHIFT and AND. 
It computes parameters for SHIFT and AND only once "at compile time" in *generate* call.
Almost all bitfield accesses in real world use symbol constants for field names.
Using multiple dispatch, Julia compiler identifies the access method for a field given as
symbol constant at compile time and inlines the SHIFT and AND operation - all field 
identification effort is "compiled away".

# Any reasons not to use *generate*?

Not really. *generate* defines additional n methods for a BitStruct having
n bitfields. Its calculation overhead roughly equals the calculation of the
SHIFT and AND parameters of the last bitfield of a BitStruct. 

Resource consumption is neglible, and you will have overall runtime
advantages after a couple of field accesses per BitStruct type.
Call *generate* if runtime performance has any relevance for your application.

# Why not implemented in macro @bitstruct or as @generated function?
    
It does not work in an acceptable fashion, due to the so called "world age" problem.
A macro and a generated function are compiled at its first use. They can
access the functions (and its methods) only in the state they are at
compile time of the macro/@generated function. 

Using macro or @generated, would imply you have to define ALL types used as
a bitfield type anywhere in a BitStruct in your whole application, including
the 'bitsizeof' method for these types. If your application uses two modules
A and B, each having enums or custom types used for bitfields, you are lost:
the first *using* or *import* statement for A or B will trigger a BitStruct
compilation, at least the other module will encounter "world age" errors.

In contrast to a macro or a @generated function, *generate* is an ordinary
(generic) julia function, and you decide in which world of method tables
it operates, by the sequence of operations in your program and the
location where you place the *generate* call.
"""
function generate end


function generate(bsType::DataType) 
    bsType <: BitStruct || throw(ArgumentError("Parameters must be a concrete BitStruct type, but is $bsType"))
    bitsize = bitsizeof(bsType)
    bitsize <= 64 || throw(ArgumentError("$bsType is invalid - has too many bits (max is 64): $bitsize"))

    types = bsType.parameters[1].parameters[2].parameters
    syms = bsType.parameters[1].parameters[1]
    shift = 0
    idx = 1
    while idx <= length(syms)
        sym = syms[idx]
        type = types[idx]
        bits = bitsizeof(type)
        strsym=string(sym)
        R = typeof(decode(type, zero(UInt64)))
        ex = :(@inline function _fielddescr(::Type{$bsType}, ::Val{Symbol($strsym)}) 
        return ($type, $shift, $bits, $idx, $R)
        end)
        #println("about to compile: ",ex)
        eval(ex) # this compiles the specialized function
        shift += bits
        idx += 1
    end
    ex = :(@inline function _fielddescr(::Type{$bsType}, s::Symbol) 
        return _fielddescr($bsType, Val(s))
        end)
    #println("about to compile: ",ex)
    eval(ex) # this compiles the specialized function with symbol argument
    return nothing
end


function generate(m::Module)
    for n in names(m)
        try # if getfield fails
            t = getfield(m, n)
            if t <: BitStruct
                #println("generate name=",n,", type= ",t)
                generate(t)
            end
        catch
        end
    end
end


#=
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
=#

function Base.string(x::BitStruct{T}) where T<:NamedTuple
    types = Tuple(T.parameters[2].parameters)
    syms = T.parameters[1]
    io = IOBuffer()
    #print(io,string(BitStruct{T}),'(')
    print(io,"BitStruct(")
    for i in 1:length(syms)
        delim = i==length(syms) ? ')' : ','
        print(io,getproperty(x,syms[i]),delim)
    end
    #close(io)b
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
        if typeof(v) <: BitStruct
            println("  ",s, ": ",repr(getproperty(x,s)))
        else
            println("  ",s, ":",typeof(v),' ',repr(getproperty(x,s)))
        end
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
            t,shift, bits,idx, R = _fielddescr(BitStruct{T},s)
            v = getproperty(x,s)
            print(io,indent2,s," [",shift+1,':',shift+bits,"::",t, "]: ")
            Base.dump(io, v,n-1,indent2)
            typeof(v) <: BitStruct || println(io) # TODO get rid off confition
        end
    end
    nothing
end


"""
setproperty(x::BitStruct{T};s::Symbol,v)
setproperty(x::BitStruct{T};s::Symbol,v::BitStruct{T})
setproperty(x::BitStruct{T};kwargs...)

return a new BitStruct which equals x in all fields but s.

Field s is replaced by v in the returned BitStruct.
If v isa BitStruct{T}, field s is replaced by getproperty(v,s)

The variant with keywords is for convenience, if runtime performance is
not important - it involves memory allocation and is slow.

Copying a field from a BitStruct of the same type is the fastest variant.
"""
@inline function setproperty(x::BitStruct{T},s::Symbol,v) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    t,shift, bits, idx, R = _fielddescr(BitStruct{T},s)
    u = encode(t,v)
    #ret = _set(ret,Val(shift),Val(bits),u)
    ret = _set(ret,shift,bits,u)
    return reinterpret(BitStruct{T},ret)
end



# optimized code when copying a field from another BitStruct instance
@inline function setproperty(x::BitStruct{T},s::Symbol,v::BitStruct{T}) where {T<:NamedTuple}
    t,shift, bits, idx, R = _fielddescr(BitStruct{T},s)
    mask = _mask(bits) << shift
    u = reinterpret(UInt64,x) & mask
    ret = (reinterpret(UInt64,x) & ~mask) | u
    return reinterpret(BitStruct{T},ret)
end


function setproperty(x::BitStruct{T};kwargs...) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    for p in pairs(kwargs)
        s = p.first
        t,shift, bits, idx, R = _fielddescr(BitStruct{T},s)
        v = encode(t,p.second)
        #ret = _set(ret,Val(shift),Val(bits),v)
        ret = _set(ret,shift,bits,v)
    end
    return reinterpret(BitStruct{T},ret)
end


# for benchmarking, only
#=
function setm2(x::BitStruct{T};kwargs...) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    nt = kwargs.data # the named tupla
    syms = typeof(nt).parameters[1]
    idx = 1
    while idx <= length(syms)
        s = syms[idx]
        t,shift, bits = _fielddescr(BitStruct{T},s)
        v = encode(t,nt[idx])
        #ret = _set(ret,Val(shift),Val(bits),v)
        ret = _set(ret,shift,bits,v)
    end
    return reinterpret(BitStruct{T},ret)
end
=#

## overloaded operators


@inline function Base.getproperty(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits, idx, R = _fielddescr(BitStruct{T},s)
    return @inbounds decode(type,_get(reinterpret(UInt64,x),shift,bits))
end


"""
    bs::BitStruct{T}   /   (fld,value)::Tuple{Symbol,Any}

fld must be a property of BitStruct{T} (a symbol contained in T, identifying a field).
The operation / replaces the bitfield fld in bs with value and returns the result.

In addition, the syntax ´bs /= fld,value´ is supported for a variable bs of 
type BitStruct{T}, it stores the result in bs.

value is either a BitStruct{T} or a value supported by [`encode`](@ref)
for the bitfield type of field fld. If value is a BitStruct{T}, its fld field
is used as value. If bs, bs2 are BitStruct{T} instances, 
*bs / (:fld,bs2)* is a shortcut for *bs / (:fld,bs2.fld)*
"""
@inline function Base.:(/)(x::BitStruct{T},t::Tuple{Symbol,Any}) where {T<:NamedTuple}
    setproperty(x,t[1],t[2])
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
    type,shift,bits, idx,R = _fielddescr(BitStruct{T},fld)
    return reinterpret(BitStruct{T},reinterpret(UInt64,x)| (_mask(bits) << shift))
end


"""
    x::BitStruct{T}   -   fld::Symbol

fld must be property of BitStruct{T} (a symbol contained in T, identifying a field).

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
    type,shift,bits, idx, R = _fielddescr(BitStruct{T},fld)
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

# struct-alike constructor
"""
    BitStruct{T}(args...;kwargs...)

Convenience constructor for a BitStruct. 

You pass field values in declared order, for n given values, the first n
fields of the BitStruct are assigned. Remaining fields are zero-initialized
(all bits are zero).

In addition, you can give a keyword argument list with *fieldname=value* syntax,
specifically setting some fields. List is processed from left to right, it is 
possible to overwrite already set fields with keyword notation.

If no arguments at all are given, all bits are unset. 
If any argument is given, constructor will allocate helper memory
and process all values as type Any - in other words, it is slow.

For a fast construction, use the constructor with no arguments, and set
all fields by [`setproperty`](@ref) or use operator / for that
(same operation, but shorter syntax).
"""
function BitStruct{T}(args...;kwargs...) where T <:NamedTuple
    ret = reinterpret(BitStruct{T},zero(UInt64))
    syms = T.parameters[1]
    for (i,v) in enumerate(args)
        ret /= (syms[i],v)
    end
    for p in kwargs
        ret /= (p.first,p.second)
    end
    return ret
end

# BitStruct has a zero value ...
Base.zero(::Type{BitStruct{T}}) where T <:NamedTuple = reinterpret(BitStruct{T},zero(UInt64))

# this is more specific than BitStruct{T}() ==> stack overflow. why??!
#function BitStruct{T}(;kwargs...) where {T<:NamedTuple}
#    set(reinterpret(BitStruct{T},zero(UInt64));kwargs...)
#end



"""
    @BitStruct name begin key1::Type1; key2::Type2; ...; end
 or @BitStruct name {key1::Type1, key2::Type2, ...}

Delimiter ';' between two field declarations can be replaced by a newline sequence, closely
resembling the usual struct syntax.

This macro defines a concrete BitStruct type with the given fields and bitfield types.
Syntax is that of [`@NamedTuple`](@ref), except the leading name.

name is an identifier, it becomes a constant to be used as short type name 
for the constructed BitStruct type. 

It follows a block with a sequence of ident::type declarations,
ident is the field name in the BitStruct, and type its declared
bitfield type. Bitfield type means: it is a type identifying the number of bits
used in the BitStruct, and it has associated conversion methods which convert
a bitfield value to and from the data type which is accessed
by field access. bitfield type can be identical to the external type, e. g.
Bool. Bit in many cases, the bitfield type differs from the external type,
it is used only to define the bitfield size and identify the conversions.

A bitfield type must be an isbits data type. Parameterized data types are allowed. 
Included is support for all predefined primitive number types up to 32 bit
sizes, Bool (consuming 1 bit), Enum subtypes (bit size automatically derived),
and Char.

For BitStruct-s memory efficiency it is essential, that field definitions
can restrict the set of instances of a Type R to a small subset, which can 
be encoded with less bits. For this purpose, special singleton types
are introduced als bitfield types. 
Each such singleton type represents an encoding/decoding
scheme for an instance subset. You can define several types T1, T2 ...
identifying distinct subrange encodings of a type R.

For Integer subranges, the singleton types BInt{N} and BUInt{N} are defined in 
BitStructs package. BInt{N} declares an Int subrange of N bits, BUInt{N} an
UInt subrange. For Characters, the frequently used subranges of ASCII and Latin-1
characters are supported with AsciiChar and LatinChar, consuming 7/8 bits in a
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

All bitfield types must be already defined when @bitstruct is called. 

The sum of bit sizes of all fields must be smaller or equal to 64, because a BitStruct 
instance is implemented as a 64 bit primitive type. Due to "world age" problems,
this macro cannot check it, but  [`generate`](@ref) does.

You should call [`generate`](@ref) to get good performance. 

To construct Instances, best use the no-args-constructor. Its fast and returns
a BitStruct with all bits set to 0. Then change field values via [`setproperty`](@ref)
or use the more concise syntax *aBitstruct /= (aBitfieldSymbol, valueToSet)*
For convenience, there is a constructor which emulates the default constructor 
of a struct (parameters are the values to assign to its fields, in declared order),
**but** it is about factor 1000 slower than the recommended approach, because it needs to 
allocate temporary structures.

The following example uses 64 bit. An ordinary struct would consume 21 bytes, 
more than doubling memory consumption. Concerning its runtime performance, have a look at the
benchmarks supplied as test functions.

```jldoctest
julia> @bitstruct MyBitStruct begin
    flag1 :: Bool
    flag2 :: Bool # many flags push memory savings and runtime advantages of a BitStruct
    flag3 :: Bool # you could always use Bool for a one-bit field, but ...
    bit1  :: BInt{1} # in a numerical context, BInt/BUInt does the number conversion for you. bit1 can be -1, 0
    ubit1  :: BUInt{1} # in a numerical context, BInt/BUInt does the number conversion for you. ubit1 can be 0,1
    ac :: AsciiChar # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    lc :: Latin1Char # similar use 8 bits for the Latin-1 character set
    id1 :: BUInt{10} # 0..1023
    id2 :: BUInt{12} # 0..4095
    delta1 :: BInt{11} # -1024:1023
    delta2 :: BInt{11} # -1024:1023
end

BitStruct{NamedTuple{(:flag1, :flag2, :flag3, :bit1, :ubit1, :ac, :lc, :id1, :id2, :delta1, :delta2), Tuple{Bool, Bool, Bool, BInt{1}, BUInt{1}, AsciiChar, Latin1Char, BUInt{10}, BUInt{12}, BInt{11}, BInt{11}}}}
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
    type,shift,bits,idx,R = _fielddescr(BitStruct{T},lastsym)
    return shift+bits
end




function decode(::Type{BitStruct{T}},v::UInt64) where T
    @boundscheck checkbitsize(v,bitsizeof(BitStruct{T}))
    return reinterpret(BitStruct{T},v)
end

@inline function encode(::Type{BitStruct{T}}, v::BitStruct{T}) where {T<:NamedTuple}
    return reinterpret(UInt64,v)
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
        shift += bitsizeof(types[i]) # throws exception if bitsizeof is undefined for some type
    end
    shift <= 64 || throw(DomainError(BitStruct{T},"Total bitsize is $shift - does not fit into 64 bit"))
end


# required by dump(aBitStructType) w/o overloading. Still necessary??!
# TODO is not correct: bitfield types are not regurned by getproperty
#function Base.datatype_fieldtypes(::Type{BitStruct{T}}) where T
#    T.parameters[2].parameters
#end

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

