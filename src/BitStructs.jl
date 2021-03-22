module BitStructs
#
include("fieldtypes.jl")
export BInt, BUInt, bitsizeof,  AsciiChar, Latin1Char

include("bitstruct.jl")
export BitStruct, set, @bitstruct

end # module