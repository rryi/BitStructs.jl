# IO support for BitStructs

function Base.read(io::IO, ::Type{BitStruct{T}}) where {T<:NamedTuple}
    bytesize = div(bitsizeof(BitStruct{T})+7,8)
    local u :: UInt64 = zero(UInt64)
    if bytesize === 8
        u = ltoh(read(io,UInt64))
    else
        if bytesize>= 4
            u += ltoh(read(io,UInt8))
            bytesize -= 4
        end
        for i in 1:bytesize
            u = (u<<8) + read(io,UInt8)
        end
    end
    return reinterpret(BitStruct{T},u) # TODO clear unused bits - just for safety??!
end

# reads in network byte ordering
function Base.read(io::IO, ::Type{BitStruct{T}}) where {T<:NamedTuple}
    bytesize = div(bitsizeof(BitStruct{T})+7,8)
    local u :: UInt64 = zero(UInt64)
    if bytesize === 8
        u = ntoh(read(io,UInt64))
    else
        if bytesize>= 4
            u = (ntoh(read(io,UInt32))) % UInt64
            bytesize -= 4
        end
        for i in 1:bytesize
            u = (u<<8) + read(io,UInt8)
        end
    end
    return reinterpret(BitStruct{T},u) # TODO clear unused bits - just for safety??!
end


# writes in network byte ordering
function Base.write(io::IO, bs :: BitStruct{T}) where {T<:NamedTuple}
    bytesize = div(bitsizeof(BitStruct{T})+7,8)
    local u :: UInt64 = reinterpret(BitStruct{T},bs)
    if bytesize === 8
        write(io, htol(u)
    else
        if bytesize>= 4
            write(io, htol(u%UInt32))
            u >>>= 32
            bytesize -= 4
        end
        for i in 1:bytesize
            write(io, u%UInt8)
            u >>>= 8
        end
    end
    return nothing
end


"""
A store for up to 7 bits.
    
readbits and writebits use it as a buffer of incomplete bytes.

In readbits context, BitQueue stores bits read from io but not consumed by a BitStruct.
In writebits context, BitQueue stores remaining bits from the last BitStruct written which did not fill up a byte.

"""
struct BitQueue
    bitcount :: UInt8
    bits :: UInt8
end


# reads in network byte ordering
function readbits(bq::BitQueue, io::IO, ::Type{BitStruct{T}}) where {T<:NamedTuple}
    local u :: UInt64 = zero(UInt64)
    local bitremainder :: UInt64
    bitsize = bitsizeof(BitStruct{T})
    bytestoread = div((bitsize-bq.bitcount+7),8)
    newbitcount = bytestoread*8+bq.bitcount-bitsize
    newbitmask = ((1<<newbitcount)-1)%UInt64
    if bytestoread === 8
        # special case: 64 bit overflow
        u = ntoh(read(io,UInt64))
        bitremainder = u & newbitmask
        u >>>= newbitcount
        u >>>= 
        u = (u << bq.bitcount) + bq.bits
    else
        # all fits into 64 bit 
        u = bq.bits
        if bytestoread >= 4
            u = u << 32 + ltoh(read(io,UInt32))
            bytestoread -= 4
        end
        for i in 1:bytestoread
            u = (u<<8) + read(io,UInt8)
        end
        bitremainder = u & ((1<<newbitcount)-1)

    bitsize = div(bitsizeof(BitStruct{T})+7,8)
    local u :: UInt64 = zero(UInt64)
    if bytesize === 8
        u = ltoh(read(io,UInt64))
    else
        if bytesize>= 4
            u = (ltoh(read(io,UInt32))) % UInt64
            bytesize -= 4
        end
        for i in 1:bytesize
            u = (u<<8) + read(io,UInt8)
        end
    end
    return reinterpret(BitStruct{T},u) # TODO clear unused bits - just for safety??!

    
