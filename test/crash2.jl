using BitStructs




"""
BS is a BitStruct designed to demonstrate BitStruct advantages.

As such, it has many small fields and two fields for data types which were not designed 
for memory efficiency (Strange,Sign), demonstrating how a custom encoding can overcome memory deficiencies.
"""
@bitstruct BS begin
    #status :: ProcStatus # could be the overall status of some process
    #strange :: Strange 
    #sign :: Sign
    flag1 :: Bool
    flag2 :: Bool
    #flag3 :: Bool # to be honest: many flags push memory savings and runtime advantages of this BitStruct
    #flag4 :: Bool # you could always use Bool for a one-bit field, but ...
    #bit1  :: BInt{1} # in a numerical context, BInt/BUInt does the number conversion for you bit1 can be -1, 0
    #ac :: AsciiChar # if you know a Char is ASCII, encode it in 7 instead of 32 bits
    #lc :: Latin1Char # similar use 8 bits for the Latin-1 character set
    id1 :: BUInt{9} # 0..511
    id2 :: BUInt{12} # 0..4095
    #delta1 :: BInt{9} # -256..255
    #delta2 :: BInt{9} # -256..255 
end

bs = BS(true,false,0x1,0x2)

show(bs)

@noinline function set2fields(s)
    if typeof(s) <: BitStruct
        s /= :id1, s.id2
        s /= :flag1, s.flag2
    else
        s.id1 = s.id2
        s.flag1 = s.flag2
    end
end

println("the statements executed by set2fields run, if executed directly")
bs /= :id1, bs.id2
bs /= :flag1,bs.flag2
show(bs)
println("Calling set2fields(bs) causes a crash")
set2fields(bs)
