using Pkg
Pkg.activate(".") # needed to execute "julia.exe test\crash2.jl" in package directory without adding BitStructs to package registry
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
        bs /= :flag1, bs.flag2 # crash
end

function set2fields5(bs::T) where T <: BitStruct # no crash
    bs /= :id1, bs.id2
end

function set2fields6(bs::T) where T <: BitStruct # no crash
    bs /= :flag1, bs.flag2
end


function set2fields7(bs::T) where T <: BitStruct
    bs = BitStructs.set(bs, :id1, bs.id2)
    bs /= :flag1, bs.flag2 # crash
end

function set2fields8(bs::T) where T <: BitStruct
    bs = BitStructs.set(bs, :id1, bs.id2)
    bs = BitStructs.set(bs, :flag1, bs.flag2) # crash
end

function set2fields9(bs::T) where T <: BitStruct
    bs = BitStructs.set(bs, :flag1, bs.flag2) # crash
end

function set2fields10(bs::T) where T <: BitStruct
    bs = BitStructs.set(bs, :id1, bs.id2)
    #bs = BitStructs.set(bs, :flag1, bs.flag2) # crash, replaced by manual inlining of the call
    #=
        @inline function set(x::BitStruct{T},s::Symbol,v) where {T<:NamedTuple}
        ret = reinterpret(UInt64,x)
        t,shift, bits = _fielddescr(BitStruct{T},Val(s))
        u = encode(t,v)
        ret = _set(ret,Val(shift),Val(bits),u)
        return reinterpret(BitStruct{T},ret)
    end
    =#
    v = bs.flag2
    ret = reinterpret(UInt64,bs)
    t,shift, bits = BitStructs._fielddescr(T,Val(:flag1))
    u = BitStructs.encode(t,v)
    ret = BitStructs._set(ret,Val(shift),Val(bits),u)
    return reinterpret(T,ret)
end

function set2fields11(bs::BitStruct{T}) where T <: NamedTuple
    bs = BitStructs.set(bs, :id1, bs.id2)
    #bs = BitStructs.set(bs, :flag1, bs.flag2) # crash, replaced by manual inlining of the call
    #=
        @inline function set(x::BitStruct{T},s::Symbol,v) where {T<:NamedTuple}
        ret = reinterpret(UInt64,x)
        t,shift, bits = _fielddescr(BitStruct{T},Val(s))
        u = encode(t,v)
        ret = _set(ret,Val(shift),Val(bits),u)
        return reinterpret(BitStruct{T},ret)
    end
    =#
    v = bs.flag2
    ret = reinterpret(UInt64,bs)
    t,shift, bits = BitStructs._fielddescr(BitStruct{T},Val(:flag1))
    u = BitStructs.encode(t,v)
    ret = BitStructs._set(ret,Val(shift),Val(bits),u)
    return reinterpret(BitStruct{T},ret)
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
#set2fields7(bs) # crash in line 61, code ´bs /= :flag1, bs.flag2´
#set2fields8(bs) # crashes in line 66, code ´bs = BitStructs.set(bs, :flag1, bs.flag2)´
set2fields9(bs) # NO crash !!
set2fields10(bs) # NO crash !!
set2fields11(bs) # NO crash !!

