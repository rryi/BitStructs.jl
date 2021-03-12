using BitStructs

@bitstruct BS begin
    flag1 :: Bool
    flag2 :: Bool
    id1 :: BUInt{9} # 0..511
    id2 :: BUInt{12} # 0..4095
end

bs = BS(true,false,0x1,0x2)
#show(bs)

@noinline function set2fields(bs)
    if typeof(bs) <: BitStruct
        bs /= :id1, bs.id2
        bs /= :flag1, bs.flag2
    else
        bs.id1 = bs.id2
        bs.flag1 = bs.flag2
    end
end

@noinline function set2fields2(bs::T) where T <: BitStruct
    if typeof(bs) <: BitStruct
        bs /= :id1, bs.id2
        bs /= :flag1, bs.flag2
    else
        bs.id1 = bs.id2
        bs.flag1 = bs.flag2
    end
end

function set2fields3(bs::T) where T <: BitStruct
    if typeof(bs) <: BitStruct
        bs /= :id1, bs.id2
        bs /= :flag1, bs.flag2
    else
        bs.id1 = bs.id2
        bs.flag1 = bs.flag2
    end
end

function set2fields4(bs::T) where T <: BitStruct
        bs /= :id1, bs.id2
        bs /= :flag1, bs.flag2
end

function set2fields5(bs::T) where T <: BitStruct
    bs /= :id1, bs.id2
end

function set2fields6(bs::T) where T <: BitStruct
    bs /= :flag1, bs.flag2
end


function set2fields7(bs::T) where T <: BitStruct
    bs = BitStruct.set(bs, :id1, bs.id2)
    bs /= :flag1, bs.flag2
end

function set2fields8(bs::T) where T <: BitStruct
    println(typeof(bs))
    println(bs.id2)
    println(typeof(bs.id2))
    bs = BitStruct.set(bs, :id1, bs.id2)
    bs /= :flag1, bs.flag2
end


#println("the statements executed by set2fields run, if executed directly")
#bs /= :id1, bs.id2 # no crash
#bs /= :flag1,bs.flag2 # no crash
#show(bs)
println("Calling set2fields(bs) causes a crash")
#set2fields(bs) # crash
#set2fields2(bs) # crash
#set2fields3(bs) # crash
#set2fields4(bs) # crash
set2fields5(bs) # NO crash !!
set2fields6(bs) # NO crash !!
#set2fields7(bs) # NO crash but weird error message:
#=
ERROR: LoadError: type UnionAll has no field set
Stacktrace:
 [1] getproperty(x::Type, f::Symbol)
   @ Base .\Base.jl:28
 [2] set2fields7(bs::BitStruct{NamedTuple{(:flag1, :flag2, :id1, :id2), Tuple{Bool, Bool, BUInt{9}, BUInt{12}}}})
   @ Main ~\julia\BitStructs\test\crash2.jl:58
 [3] top-level scope
   @ ~\julia\BitStructs\test\crash2.jl:74
in expression starting at c:\Users\RR\julia\BitStructs\test\crash2.jl:74
=#

set2fields8(bs)