module BitStructs
#
include("fieldtypes.jl")
export BInt, BUInt, bitsizeof

include("bitstruct.jl")
export BitStruct, bitsizeof, @bitstruct, specialize, _fielddescr
# Tagging types for BitStructs
export BInt, BUInt, AsciiChar, Latin1Char

end # module