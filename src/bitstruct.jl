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
@Base.pure function _set(pstruct::UInt64,shift,bits,value::UInt64) 
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

This function is slow, even with multiple dispatch. It is replaced at
compile time by @BitStruct which generates specialized fast methods, 
returning a constant
"""

@inline function _fielddescr(::Type{BitStruct{T}},s::Symbol) where {T<:NamedTuple}
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


@inline function Base.getproperty(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescr(BitStruct{T},s)
    return decode(type,_get(reinterpret(UInt64,x),shift,bits))
end


function Base.show(x::BitStruct{T}) where T<:NamedTuple
    ps = reinterpret(UInt64,x)
    println(BitStruct{T}, ' ',repr(ps))
    types = Tuple(T.parameters[2].parameters)
    syms = T.parameters[1]
    for s in syms
        t,shift, bits = _fielddescr(BitStruct{T},s)
        #println("  ",s, "::",t, " , ",shift," , ",bits)
        println("  ",s, "::",t, " = ",repr(decode(t,_get(ps,shift,bits))))
    end
    println("end")
end


"""
    set(ps::BitStruct{T};s::Symbol,v)

replace field s in ps by v.
"""
function set(x::BitStruct{T},s::Symbol,v) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    t,shift, bits = _fielddescr(BitStruct{T},s)
    u = encode(v, bits)
    ret = _set(ret,Val(shift),Val(bits),u)
    return reinterpret(BitStruct{T},ret)
end


"""
    set(ps::BitStruct{T};s::Symbol,v)

replace field s in ps by v.
"""
function Base.:(/)(x::BitStruct{T},t::Tuple{Symbol,Any}) where {T<:NamedTuple}
    set(x,t[1],t[2])
end

"""
    set(ps::BitStruct{T};kwargs...)

replace a selection of fields given by named parameters.
parameter names in args must match properties of ps
"""
function set(x::BitStruct{T};kwargs...) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    for p in pairs(kwargs)
        s = p.first
        t,shift, bits = _fielddescr(BitStruct{T},s)
        v = encode(p.second, bits)
        ret = _set(ret,Val(shift),Val(bits),v)
    end
    return reinterpret(BitStruct{T},ret)
end


# for benchmarking, only
function set2(x::BitStruct{T};kwargs...) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    nt = kwargs.data # the named tupla
    syms = typeof(nt).parameters[1]
    idx = 1
    while idx <= length(syms)
        s = syms[idx]
        t,shift, bits = _fielddescr(BitStruct{T},s)
        v = encode(nt[idx], bits)
        ret = _set(ret,Val(shift),Val(bits),v)
    end
    return reinterpret(BitStruct{T},ret)
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
    type,shift,bits = _fielddescr(BitStruct{T},lastsym)
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

