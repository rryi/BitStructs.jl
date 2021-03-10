### A BitStructs tutorial as commented example code

# 1st steps

# of course you must add package BitStructs to your julia environment and import the package
using BitStructs


# enum types are very well suited as field types in BitStructs
# this one will consume only 2 bits in a BitStruct
@enum ProcStatus ::UInt8 S_WAITING S_RUNNING S_DONE S_FAILURE

# A BitStruct type is very similar to a plain julia struct:

# declare it
@bitstruct MyFirstBitStruct {aStatus::ProcStatus, aFlag::Bool, aChar::Char, aNumber::Int16}

# construct an instance
bs1st = MyFirstBitStruct(S_WAITING,true,'a',17)

# access its fields
bs1st.aFlag && println("bs1st flag is set")
println("bs1st aChar and aNumber are ",bs1st.aChar,' ',bs1st.aNumber)

# or have a complete printout for debugging
show(bs1st)

# BitStructs are immutable, but...
bs1st = BitStructs.set(bs1st; aChar='b', aNumber=18)
# it is very easy to replace some fields by other values

# / is overloaded for a BitStruct as left, and a tuple as right operand:
bs1st = bs1st / (:aChar,'c')
show(bs1st)
# this allows a really compact notation for "writing to a field":
bs1st /= :aFlag, false
show(bs1st)

# so far with 1st steps 


## defining BitStruct types

# for safety and readability, always use @bitstruct
# @bitstruct verifies all types used in declaration are permitted 
# @bitstruct verifies bitsize sum of all fields does not exceed 64

# @bitstruct constructs a BitStruct type and defines an alias for it as const
@bitstruct MiniBS {flag::Bool, ch::Char, val::Int16}
println("MiniBS = ",MiniBS)

# BitStruct has some predefined types to be used only in BitStruct
# type declarations. They cover important subranges
# of julia primitive types Int64, UInt64, Char in less bits.
# They are BInt{N}, BUInt{N}, AsciiChar, Latin1Char. See Doc on these types
@bitstruct TagTypeBS {ac::AsciiChar, lc::Latin1Char, i6bits::BInt{6}, u11bits::BUInt{11}}

# Types can be customized for use in BitStructs.
# Here is an Enum which is not so well designed, with respect to BitStructs:
@enum Strange ::Int BIGMINUS=-999999 ONE=1 TWO=2 THREE=3

# You can use it as is in a BitStruct. However it will consume 20 bits.
println("bits used by Strange as BitStruct field:", bitsizeof(Strange))

# with a custom encoding, it reduces to 2 bits:
BitStructs.bitsizeof(::Type{Strange}) = 2
BitStructs.encode(v::Strange) = (v==BIGMINUS ? zero(UInt64) : Int(v)%UInt64)
BitStructs.decode(::Type{Strange},u::UInt64) = (u==zero(UInt64) ? BIGMINUS : StrangeEncoded(u))
println("bits used by Strange with custom encoding:", bitsizeof(Strange))

# To encode a small subset of an existing type efficiently, 
# define a singleton type and a custom encoding. Example:
# consider you want to store a sign of a (real) number.
# It should have the values -1.0 and 1.0,
# so that you can apply the sign by a multiplication.
struct Sign end
BitStructs.bitsizeof(::Type{Sign}) = 1
BitStructs.encode(::Type{Sign},v::Float64) = (v.value>0 ? zero(UInt64) : one(UInt64))
BitStructs.decode(::Type{Sign},u::UInt64) = (u==zero(UInt64) ? 1.0 : -1.0)



# we conclude this chapter with a 'big' BitStruct using all discussed types.
# BitStructs with many fields and comments are better readable with block syntax: 
@bitstruct MaxiBS begin
    status :: ProcStatus # could be the overall status of some process
    strange :: Strange 
    sign :: Sign
    flag1 :: Bool
    flag2 :: Bool
    flag3 :: Bool # to be honest: many flags push memory savings and runtime advantages of this BitStruct
    flag4 :: Bool # you could always use Bool for a one-bit field, but ...
    bit1  :: BInt{1} # in a numerical context, BInt/BUInt does the number conversion for you
    ac :: AsciiChar # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    lc :: Latin1Char # similar use 8 bits for the Latin-1 character set
    id1 :: BUInt{9} # 0..511
    id2 :: BUInt{12} # 0..4095
    delta1 :: BInt{9} # -256..255
    delta2 :: BInt{9} # -256..255 
end
