using BitStructs
using BenchmarkTools





## some simple field access benchmarks to begin with

mutable struct T
    id1::UInt16
    id2::UInt16
    flag1::Bool
    flag2::Bool
end
Base.copy(x::T) = T([getfield(x, k) for k ∈ fieldnames(T)]...)

@bitstruct BT begin
    id1::BUInt{16}
    id2::BUInt{16}
    flag1::Bool
    flag2::Bool
end

t = T(5%UInt16, 6%UInt16, true, false)
bt = BT(5%UInt16, 6%UInt16, true, false)




#@btime $t.i1,$t.i2,$t.f1,$t.f2
# this is optimized away, totally
#@btime $bt.i1,$bt.i2,$bt.f1,$bt.f2


#@noinline bench1(t) = (t.i1+t.i2,t.f1,t.f2)
#@btime bench1($t)
#@btime bench1($bt)

@noinline function set2fields(s)
    if typeof(s) <: BitStruct
        s /= :id1, s.id2
        s /= :flag1, s.flag2
    else
        s.id1 = s.id2
        s.flag1 = s.flag2
    end
end

tc = copy(t)
println("set 2 fields struct")
@btime set2fields($tc)
println("set 2 fields BitStruct")
@btime set2fields($bt)


# drill down on time consume setting a field


println("b1 set field by /=")
@noinline function b1(b::T) where T <: BitStruct
    s = 0%UInt64
    for i in zero(UInt64):UInt64(100)
        b /= (:id1, i)
        s += b.id1
    end        
    return s
end
@btime (b1($bt))


println("b2 set field by set(..)")

@noinline function b2(b::T) where T <: BitStruct
    s = 0%UInt64
    for i in zero(UInt64):UInt64(100)
        b = BitStructs.set(b,:id1, i)
        s += b.id1
    end        
    return s
end
@btime (b2($bt))


@inline function setNoEncode(x::BitStruct{T},s::Symbol,v) where {T<:NamedTuple}
    ret = reinterpret(UInt64,x)
    t,shift, bits = BitStructs._fielddescr(BitStruct{T},Val(s))
    # u = 1%UInt64 # encode(t,v)
    u = BitStructs.encode(t,v)
    ret = BitStructs._set(ret,Val(shift),Val(bits),u)
    return reinterpret(BitStruct{T},ret)
end


println("only encode")
@noinline function b3(b::T) where T <: BitStruct
    s = 0%UInt64
    for i in zero(UInt64):UInt64(100)
        b = setNoEncode(b,:id2, i)
        s += b.id1
    end        
    return s
end
@btime (b3($bt))

println("done")
