# array-like structures using BitStruct-s

mutable struct BSTable{BitStruct{T}} <: AbstractVector{BitStruct{T}} where {T<:NamedTuple}
    base:: Vector{BitStruct{T}}
end
