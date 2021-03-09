module BitStructs
#
include("fieldtypes.jl")
export BInt, BUInt, bitsizeof

include("bitstruct.jl")
export BitStruct, set, @bitstruct, @bs

end # module