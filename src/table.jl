# array-like structures using BitStruct-s
using Tables

"""
BSTable{T} is a wrapped vector of BitStruct{T}.
Methods are defined which make it conforming to a
rows-based table as specified in Tables.jl.

BSTable is a dense table structure with superb memory 
efficiency, due to BitStruct type compression - provided you can
make use of data types with small or restricted instance sets.

BSTable read and write operations are superfast, in particular
operations on a row record are of outstanding performance.

The downside: compared to other Tables.jl conformant table 
structures, BSTable is quite inflexible: you cannot change 
the column structure dynamically, and, probably most 
restrictive, you are limited to 64 bit for the whole
(BitStruct-compressed) row.

To overcome those restrictions, BitStructs package offers the 
option to use a subset of columns of a BSTable as columns in 
another columns-based table structure like TypedTables.jl or 
Dataframes.jl - [`BSColumn `](@ref) provides an AbstractVector 
interface to a BSTable column for that purpose.

Wrapping has following reasons: 

 -  a pure Vector{BStruct} should not automatically act as a table

 -  BitStructs is a quite young package. Maybe we want to add 
    more fields to a BSTable later on

 -  we do not want to redefine lots of Vector methods to implement 
    additional features needed for table use, like resize!, 
    append!, push! and more, which would be required for the following 
    reason

 -  BSTable columns can be used as part of another table implementation,
    like DataFrames.jl or TypedTables.jl. Resizing these columns needs
    additional synchronization effort: column-based table structures resize
    by resizing every column, a BSTable detects that and acts accordingly.
    See [`BSColumn`](@ref) 
 

To keep struct small, table resize management is externalized into
into [`trackBSTableResize`](@ref).
"""
struct BSTable{T<:NamedTuple} <: AbstractVector{BitStruct{T}}
    rows:: Vector{BitStruct{T}}
end
#const BSTable{T} = Vector{BitStruct{T}}


BSTable(bs::Vector{BitStruct{T}}) where T = BSTable{T}(bs)


## Vector methods for BSTable
Base.checkbounds(::Type{Bool}, bt::BSTable{T}, idx) where T = checkbounds(Bool,bt.rows,idx)
Base.eltype(::BSTable{T}) where T <:NamedTuple = BitStruct{T}

Base.iterate(bt::BSTable{T},idx=1) where T = idx > length(bt.rows) ? nothing : (bt.rows[idx],idx+1)
Base.length(bt::BSTable{T}) where T = length(bt.rows)
Base.size(bt::BSTable{T}) where T = length(bt.rows)
Base.getindex(bt::BSTable{T} , i) where T = bt.rows[i]
Base.setindex!(bt::BSTable{T} , v, i) where T = setindex!(bt.rows,v,i)
Base.firstindex(bt::BSTable{T} , v, i) where T = 1
Base.lastindex(bt::BSTable{T} , v, i) where T = length(bt.rows)

# go for default
#function Base.similar(bt::BSTable{T} , element_type, dims) where T 
#    element_type != BitStruct{T} && return Array{element_type, dims}
#end


## mutable BSTable methods
Base.push!(bt::BSTable{T}, row::BitStruct{T}) where T = push!(bt.rows,row,i)
Base.append!(bt::BSTable{T}, rows) where T = append!(bt.rows,rows)
Base.empty!(bt::BSTable{T}) where T = empty!(bt.rows)


## following code from TypedTables, adopted for BSTable

Tables.istable(::Type{<:BSTable}) = true
Tables.isrowtable(::Type{<:BSTable}) = true
Tables.rowaccess(::Type{<:BSTable}) = true


#Tables.materializer(::BSTable) = BSTable
@inline Tables.rows(t::BSTable) = t


# we cannot use getfield because it is not redefineable for BitStruct
@inline function Tables.getcolumn(row::BitStruct{T}, i::Int) where T<:NamedTuple
    s = T.parameters[1][i]
    return getProperty(row,s)
end

function Tables.schema(bt::BSTable{T}) where T
    return Schema(fieldnames(BitStruct{T}),fieldtypes(BitStruct{T}))
end


## AbstractVector methods for table

# BSTable column methods
Tables.columnaccess(::Type{<:BSTable}) = true

# get the column corresponding to a bitstruct field name
function Base.getproperty(bt::BSTable{T}, sym::Symbol) where T
    type,shift,bits, idx, R = _fielddescr(BitStruct{T},sym) # only to check validity
    return BSColumn{sym,T,R}(bt)
end

# geh i-th column
Tables.getcolumn(bt::BSTable{T}, i) where T = return getproperty(bt,fieldname(bt,i))



"""
    columns(table::BSTable)

"""
@inline Tables.columns(bt::BSTable) = bt


function Base.setproperty!(t::BSTable, name::Symbol, a)
    error("BSTable columns are immutable. Set the values of an existing column with the `.=` operator, e.g. `table.name .= array`.")
end

#?? propertytype(::BSTable) = BSTable

"""
    columnnames(BSTable)

Return a tuple of the column names of a `BSTable`.
"""
columnnames(bt::BSTable{T}) where T = fieldnames(BitStruct{T})

# show
#Base.show(io::IO, ::MIME"text/plain", t::BSTable) = showtable(io, t)
#Base.show(io::IO, t::BSTable) = showtable(io, t)

# Basic AbstractArray interface

@inline Base.size(t::BSTable) = size(t.rows)
@inline Base.axes(t::BSTable) = axes(t.rows)
@inline Base.IndexStyle(t::BSTable) = IndexStyle(t.rows)

Base.checkbounds(::Type{Bool}, t::BSTable, i) = checkbounds(Bool,t.rows,i)


# Private fields are never exposed since they can conflict with column names
Base.propertynames(t::BSTable, private::Bool=false) = columnnames(t)


## BSColumn AbstractVector implementation


"""
BSColumn resize management.

Contains the list of externally known sizes of all BSColumn-s of a
BSTable, used on resize operations of a BSTable and its BSColumn-s.

If a BSColumn is resized, it is first checked if its row count stored 
here matches the BSTable row count. Following cases are distingushed:

 1. no entry in trackBSTableResize: create entry, set all row counts to
    row count of BSTable. Index in Int vector is index of the
    property symbol in the BStruct NamedTuple symbol list.
    Continue with case 2.

 2. row count in dict === row count of BSTable: resize BSTable, update
    row count for this BSColumn in dict to new BSTable row count.
    This is the expected case for 1st column to be resized.

 3. new length in resize === row count of BSTable: do not resize 
    BSTable, update  row count in dict to BSTable row count. This is
    the expected case for all but 1st column to be resized.

 4. any other case: throw an ArgumentError.

This tracking ensures that if several BSColumn-s of the same
BSTable are part of another table structure, resizing that table
structure will work as expected. The first BSColumn which is resized
does the resize operation of the BSTable, all following ones are
checked on conformance. If there are some properties of a BSTable
not used as BSColumn, no error will be thrown. But if a BSColumn is
used in another table, which is not synched in its row count, resize
operations will cause an error.

Lazy initialization on 1st use (case 1.) ensures that this construction 
is properly initialized even after a deserialization of a BSTable or
BSColumn.

key is the BitStruct vector instance (because it must be a heap object), 
value is the vector of the current sizes of its BSColumn instances
"""
const trackBSTableResize = Base.IdDict{Vector{BitStruct},Vector{Int}}()

"""
A type-stable, fully typed column of a BSTable, to be used in other 
type-stable table structures like TypedTables.jl

Encapsulates access to property s in its BSTable.
Read/write performance is excellent, resize operations have
some overhead: because a BSColumn is part of a BSTable, a
resize cannot simply map on a vector resize. See doc on
[`trackBSTableResize`](@ref) 
"""
struct BSColumn{s,T<:NamedTuple,R} <: AbstractVector{R}
    table :: BSTable{T}
end

function BSColumn(s::Symbol, t::BSTable{T}) where T
    t,shift, bits, idx, R = _fielddescr(BitStruct{T},s)
    return BSColumn{s,T,R}(t)
end


Base.checkbounds(::Type{Bool}, bc::BSColumn{s,T,R}, idx) where {s,T,R} = checkbounds(Bool,bc.table.rows,idx)
Base.iterate(bc::BSColumn{s,T,R}, idx=1)  where {s,T,R} = idx > length(bc.table.rows) ? nothing : (bc[idx],idx+1)
Base.eltype(bc::BSColumn{s,T,R})  where {s,T,R} = R
Base.length(bc::BSColumn{s,T,R}, idx=1)  where {s,T,R} = length(bc.table.rows)
Base.size(bc::BSColumn{s,T,R})  where {s,T,R} = size(bt.table.rows)
Base.getindex(bc::BSColumn{s,T,R},i)  where {s,T,R} = bc.table.rows[i].getproperty(s)
Base.firstindex(bc::BSColumn{s,T,R})  where {s,T,R} =  1
Base.lastindex(bc::BSColumn{s,T,R})  where {s,T,R} =  length(bc.table.rows)
function Base.setindex!(bc::BSColumn{s,T,R},v,i)  where {s,T,R} 
    r = bc.table.rows[i]
    r /= s,v
    bc.table.rows[i] = r
end

# go for default
#function Base.similar(bt::BSTable{T} , element_type, dims) where T 
#    element_type != BitStruct{T} && return Array{element_type, dims}
#end


## BSColumn resizing vector methods

# resize is the central method for any supported resizing
function Base.resize!(bc::BSColumn{s,T,R}, n::Integer)  where {s,T,R}
    bv = bc.table.rows
    lengthVec = get(trackBSTableResize,bv,Int[])
    lengthTable = length(bv) # current row count
    if length(lengthVec)===0
        # create entry 
        syms = fieldnames(bc.table)
        resize!(lengthVec,length(syms))
        fill!(lengthVec,length(bv))
    end
    t,shift, bits,idx, RR = _fielddescr(BitStruct{T},s)
    if lengthTable == lengthVec[idx]
        # 1st column to be resized - do it in table!
        resize!(bv,n)
    else
        if n != lengthTable
            # must not happen: all columns have to be resized to the same table size.
            throw(ArgumentError("Table is partly resized to $lengthTable, column $s cannot be resized to $n"))
        end
    end
    lengthVec[idx] = n
    trackBSTableResize[bv] = lengthVec
end

function Base.push!(bc::BSColumn{s,T,R},v)  where {s,T,R} 
    n = length(bc) + 1
    resize!(bc,n)
    bc[n] = v
end

function Base.append!(bc::BSColumn{s,T,R},rows)  where {s,T,R} 
    i = length(bc) 
    n = i + length(rows)
    resize!(bc,n)
    for v in rows
        i += 1
        bc[i] = v
    end
end


Base.empty!(bc::BSColumn{s,T,R})  where {s,T,R} = resize!(bc,0)


