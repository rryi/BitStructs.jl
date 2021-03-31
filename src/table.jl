# array-like structures using BitStruct-s

"""
A row-based table conforming to Tables.jl, using a vector of BitStruct-s.

To keep the struct small, table resize management is externalized.
Resize operations might be slower than those on tables based on few column 
vectors.
"""
struct BSTable{BitStruct{T}} <: AbstractVector{BitStruct{T}}
    base:: Vector{BitStruct{T}}
    #size:: Vector{Int32} # 
end


const sizesOfBSTable = Base.IdDict()

"""
A type-stable, fully typed column of a BSTable, to be used in other 
type-stable table structures like TypedTables.jl

Encapsulates access to property s in its BSTable.
Read/write performance is excellent, resize operations have
some overhead: because a BSColumn is part of a BSTable, A
resize cannot simply map on a vector resize, some  
"""
struct BSColumn{s,T<:NamedTuple,R} <= AbstractVector{R}
    table :: BSTable{BitStruct{T}}
end

