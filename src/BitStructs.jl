module BitStructs
#
include("fieldtypes.jl")
export BInt, BUInt, bitsizeof

include("bitstruct.jl")
export BitStruct, bitsizeof, @bitstruct, specialize
# Tagging types for BitStructs
export BInt, BUInt, AsciiChar, Latin1Char

end # module