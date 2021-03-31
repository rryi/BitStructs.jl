# array-like structures using BitStruct-s

mutable struct BSTable{BitStruct{T}} <: AbstractVector{BitStruct{T}}
    base:: Vector{BitStruct{T}}
    size:: Vector{Int32} # 
end

struct BSColumn{T,s} <= AbstractVector
    table :: BSTable{BitStruct{T}}
end
