using BitStructs

@bitstruct BS begin
    flag1 :: Bool
    flag2 :: Bool
    id1 :: BUInt{9} # 0..511
    id2 :: BUInt{12} # 0..4095
end

bs = BS(true,false,0x1,0x2)
#show(bs)

function set2fields4(bs::T) where T <: BitStruct
        bs /= :id1, bs.id2
        bs /= :flag1, bs.flag2
end

println("Calling set2fields(bs) causes a crash")
set2fields4(bs) # crash
