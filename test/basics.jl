using BitStructs


S1 = @NamedTuple{ f1::BUInt{1}, f2::Bool,i1::BUInt{6}, i2::BInt{8}, i3::Int8}
S2 = @NamedTuple{ v1::UInt8, v2::UInt8, v3::UInt16, v4::UInt32}


function testbasics()
    T0 = BStruct{S1}
    t0 = reinterpret(BStruct{S1},0x0000000000665544)  
end



NT = @NamedTuple{ f1::Bool, f2:: BUInt{1}, i1::BUInt{6}, i2::BInt{8}, i3::UInt8, i4::Int8, u16::UInt16, i16::Int16}
const PS = BStruct{NT}


struct S 
    f1::Bool
    f2:: UInt8
    i1::UInt8
    i2::Int8
    i3::UInt8
    i4::Int8
    u16::UInt16
    i16::Int16
end

psv = Vector{PS}(undef, 100)

sv =  Vector{S}(undef, 100)
for i in 1:length(psv)
    local ps
    ps = psv[i]
    sv[i] = S(ps.f1,ps.f2,ps.i1,ps.i2,ps.i3,ps.i4,ps.u16,ps.i16)
end


function bench(vec)
    sum = 0
    for ps in vec
        sum += ps.i1+ps.i2+ps.u16+ps.i16
    end
    sum
end


# not used
function benchV2(vec::Vector{PS}) where PS <: BStruct
    sum = 0
    for ps in vec
        sum += getpropertyV2(ps, :i1) +getpropertyV2(ps, :i2) +getpropertyV2(ps, :u16) +getpropertyV2(ps, :i16)
    end
    sum
end



# _fielddescr call replaced in getpropertiesV2 by ins result, for type PS
import BitStructs: _get, _convert

function getpropertyV3(ps, ::Val{:i1})
    type,shift,bits = BUInt{6},2,6 # _fielddescr(BStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,ps),shift,bits))    
end

function getpropertyV3(ps, ::Val{:i2})
    type,shift,bits = BInt{8},8,8 # _fielddescr(BStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,ps),shift,bits))    
end

function getpropertyV3(ps, ::Val{:u16})
    type,shift,bits = UInt16,32,16 # _fielddescr(BStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,ps),shift,bits))    
end

function getpropertyV3(ps, ::Val{:i16})
    type,shift,bits = Int16,48,16 # _fielddescr(BStruct{T},Val(s))
    return _convert(type,_get(reinterpret(UInt64,ps),shift,bits))    
end


function benchV3(vec::Vector{PS}) where PS <: BStruct
    sum = 0
    for ps in vec
        sum += getpropertyV3(ps, Val(:i1)) +getpropertyV3(ps, Val(:i2)) +getpropertyV3(ps, Val(:u16)) +getpropertyV3(ps, Val(:i16))
    end
    sum
end




# hand-coded bare metal for type PS: drilldown to elementary shift, and operation, omitting final type conversion
function getpropertyV4(ps, ::Val{:i1})
    reinterpret(UInt64,ps)>>2 & 0x3F
end

function getpropertyV4(ps, ::Val{:i2})
     ((reinterpret(UInt64,ps)>>8 & 0xFF)%Int64)<<(64-8)>>(64-8)
end

function getpropertyV4(ps, ::Val{:u16})
    reinterpret(UInt64,ps)>>32 & 0xFFFF
end

function getpropertyV4(ps, ::Val{:i16})
    Int16( ((reinterpret(UInt64,ps)>>48 & 0xFFFF)%Int64)<<(64-16)>>(64-16)  )
end

function benchV4(vec::Vector{PS}) where PS <: BStruct
    sum = 0
    for ps in vec
        sum += getpropertyV4(ps, Val(:i1)) + getpropertyV4(ps, Val(:i2)) +getpropertyV4(ps, Val(:u16)) +getpropertyV4(ps, Val(:i16))
    end
    sum
end









using BenchmarkTools

println("@btime bench(sv): some work on an ordinary struct, in a loop on a Vector to get stable timings")

@btime bench($sv)

println("@btime bench(psv): same work on BStruct having same fields as struct in preceding benchmark")
@btime bench($psv)

#println("@btime benchV2(psv): same work, but using getpropertyV2 instead of getproperty for BStruct field access")
#@btime benchV2($psv)

println("@btime benchV3(psv): same work, but handcoded getpropertyV3 replacing _fielddescr call by its result (simulated constant propagation)")
@btime benchV3($psv)

println("@btime benchV4(psv): same work, but handcoded getpropertyV4 with resulting SHIFT and AND operation")
@btime benchV4($psv)





# constructor tests

nv = (f1=false,f2=1,i1=1%UInt64,i2=2%Int64,i3=0x3,i4=4%Int8,u16=0x0123,i16=2345%Int16)

ps = PS(nv)

show(ps)

b1(str::PS)= str.i16
#b2(str::PS)= getpropertyV2(str, :i16)
b3(str::PS)= getpropertyV3(str, Val(:i16))
b4(str::PS)= getpropertyV4(str, Val(:i16))
@btime (b1($ps))
#@btime (b2($ps))
@btime (b3($ps))
@btime (b4($ps))



#@code_native(getproperty(ps,:i16)) # quite long

#@code_native(BitStructs._fielddescr(PS,Val(:i16))) # quite long


