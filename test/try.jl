module BitStructs


NT = @NamedTuple{ f1::Bool, f2:: BUInt{1}, i1::BUInt{6}, i2::BInt{8}, i3::UInt8, i4::Int8, u16::UInt16, i16::Int16}
const BS = BitStruct{NT}

bs = BS(;i1=5,i3=8)



s="""
function _fielddescr1(::Type{BitStruct{T}},::Val{s}) where {T<:NamedTuple, s}
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
"""
Meta.parse(s)


s=
"""
@inline function Base.getproperty(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = _fielddescr(BitStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end

"""


s=
"""
@inline function Base.getproperty(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = 1,2,3
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end"""


type=1
shift = 2
bits = 3

s="@inline Base.getproperty(x::BitStruct{T},Val{s}) where {T<:NamedTuple, s} = _convert($type,_get(reinterpret(UInt64,x),$shift,$bits))"

julia> Meta.parse(s)

:(#= none:1 =# @inline (Base.getproperty(x::BitStruct{T}, Val{s}) where {T <: NamedTuple, s}) = begin
              #= none:1 =#
              _convert(1, _get(reinterpret(UInt64, x), 2, 3))
          end)



end