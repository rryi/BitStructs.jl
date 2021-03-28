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
        bytesread = 0
        if bytesize>= 4
            u = (ntoh(read(io,UInt32))) % UInt64
            bytesread = 4
        end
        for i in bytesread:(bytesize-1)
            u += ((read(io,UInt8)%UInt64) << (8*i))
        end
    end
    return reinterpret(BitStruct{T},u) # TODO clear unused bits - just for safety??!
end


# writes in network byte ordering
function Base.write(io::IO, bs :: BitStruct{T}) where {T<:NamedTuple}
    bytesize = div(bitsizeof(BitStruct{T})+7,8)
    local u :: UInt64 = reinterpret(BitStruct{T},bs)
    if bytesize === 8
        write(io, htol(u))
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


"""
readbits(bq::BitQueue, io::IO, BitStruct{T}) :: Tuple(BitQueue, BitStruct{T})
readbits(io::IO, BitStruct{T}) :: Tuple(BitQueue, BitStruct{T})

Consume exactly bitsizeof(BitStruct{T}) bits from (bq,io) and 
return a new BitQueue with read but not consumed bits and a BitStruct{}
built from consumed bits.

If parameter bq is missing, it is  

"""
function readbits(bq::BitQueue, io::IO, ::Type{BitStruct{T}}) where {T<:NamedTuple}
    local u :: UInt64 = zero(UInt64)
    local bitremainder :: UInt64
    bitsize = bitsizeof(BitStruct{T})
    bytestoread = div((bitsize-bq.bitcount+7),8)
    newbitcount = bytestoread*8+bq.bitcount-bitsize
    bytesread = 0
    if bytestoread === 8
        u = ntoh(read(io,UInt64))
    else
        if bytestoread >= 4
            u = ltoh(read(io,UInt32)) %UInt64
            bytesread = 4
        end
        for i in bytesread:(bytestoread-1)
            u += ((read(io,UInt8)%UInt64) << (8*i))
        end
    end
    bitremainder = u >>> (bytestoread*8-newbitcount) 
    u = ((u << bq.bitcount) + bq.bits) & ((1<<bizsize)-1) # overflow in bitmask for bitsize==64 is ok!
    return BitQueue(newbitcount,bitremainder) , reinterpret(BitStruct{T},u) # TODO clear unused bits - just for safety??!
end
    
"""
readbits(bq::BitQueue, io::IO, BitStruct{T}) :: Tuple(BitQueue, BitStruct{T})
readbits(io::IO, BitStruct{T}) :: Tuple(BitQueue, BitStruct{T})

Consume exactly bitsizeof(BitStruct{T}) bits from (bq,io) and 
return a new BitQueue with read but not consumed bits and a BitStruct{}
built from consumed bits.

If there are no unconsumed bits, you can call the method without parameter bq
"""
function readbits(bq::BitQueue, io::IO, ::Type{BitStruct{T}}) where {T<:NamedTuple}
    local u :: UInt64 = zero(UInt64)
    local bitremainder :: UInt64
    bitsize = bitsizeof(BitStruct{T})
    bytestoread = div((bitsize-bq.bitcount+7),8)
    newbitcount = bytestoread*8+bq.bitcount-bitsize
    bytesread = 0
    if bytestoread === 8
        u = ntoh(read(io,UInt64))
    else
        if bytestoread >= 4
            u = ltoh(read(io,UInt32)) %UInt64
            bytesread = 4
        end
        for i in bytesread:(bytestoread-1)
            u += ((read(io,UInt8)%UInt64) << (8*i))
        end
    end
    bitremainder = u >>> (bytestoread*8-newbitcount) 
    u = ((u << bq.bitcount) + bq.bits) & ((1<<bizsize)-1) # overflow in bitmask for bitsize==64 is ok!
    return BitQueue(newbitcount,bitremainder) , reinterpret(BitStruct{T},u) # TODO clear unused bits - just for safety??!
end
    
readbits(io::IO, ::Type{BitStruct{T}}) where {T<:NamedTuple} = readbits(BitQueue(0,0),io,BitStruct{T})

    
"""
writebits(bq::BitQueue, io::IO, bs::BitStruct{T}) :: BitQueue
writebits(io::IO, bs::BitStruct{T}) :: BitQueue

write bs with exactly bitsizeof(BitStruct{T}) bits to io.
Because IO is a byte-oriented stream, sub-byte-quantities must be buffered.
Ihis is performed by parameter bq. It contains bits from preceding writebits
operations, which were not written to io. 

On return, the last bits of bs which do not fill up to a complete byte,
are returned as BitQueue, to be written to io in the next call of writebits.

If there are no unwritten bits from preceding writebits calls, 
you can call the method without parameter bq
"""
function writebits(bq::BitQueue, io::IO, bs::BitStruct{T}) where {T<:NamedTuple}
    local u :: UInt64 = reinterpret(UInt64,bs)
    local bitremainder :: UInt64
    bitsize = bitsizeof(BitStruct{T})
    bytestowrite = div((bitsize+bq.bitcount),8)
    newbitcount = rem((bitsize+bq.bitcount),8)
    v = u << bq.bitcount + bq.bits
    if bytestowrite === 0
        bitremainder = v
    else
        bitremainder = (u >>> (bytestowrite*8-bq.bitcount)) # v might not contain all bits to enqueue
        byteswritten = 0
        if bytestowrite === 8
            write(io, hton(v))
        else
            if bytestowrite >= 4
                write(io, hton(v)%UInt32)
                v >>>= 32
                byteswritten = 4
            end
            for i in (byteswritten+1):bytestowrite
                write(io, v%UInt8)
                v >>>= 8
            end
        end
    end
    @boundscheck bitremainder< (1<<newbitcount) || throw(BoundsError("")
    return BitQueue(newbitcount%UInt8,bitremainder%UInt8)
end
    
writebits(io::IO, bs::BitStruct{T}) where {T<:NamedTuple} = writebits(BitQueue(0,0),io,bs)
