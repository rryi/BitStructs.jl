using Pkg
Pkg.activate(".") # needed to execute "julia.exe test\crash.jl" in package directory without adding BitStructs to package registry
using BitStructs

@bitstruct BS begin
    flag1 :: Bool
    flag2 :: Bool
    id1 :: BUInt{9} # 0..511
    id2 :: BUInt{12} # 0..4095
end

function set2fields8(bs::T) where T <: BitStruct
    bs = BitStructs.set(bs, :id1, bs.id2)
    bs = BitStructs.set(bs, :flag1, bs.flag2) # crash
end

bs = BS(true,false,0x1,0x2)
set2fields8(bs) # crash
