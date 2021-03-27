
function testWriteProperties()


    bf = BFlags()
    bf /= :f1, true
    @test bf.f1 
    bf = BFlags()
    bf /= :f2, true
    @test bf.f2
    bf = BFlags()
    bf /= :f3, true
    @test bf.f3

    ba1 = BSArg(bf,S_DONE,7%UInt64,-7)
    ba2 = BSArg(BFlags(),S_FAILURE,1023%UInt64,-256)
    bs = BS()
    bs = setproperty(bs,:first,ba1)
    @test bs.first === ba1
    bs2 = setproperty(BS(),:first,bs)
    @test bs1.first === ba1

    bs = setproperty(BS(),f=bf,sign=0.0,strange=ONE,bit=0,ac='b',first=ba1)
    @test bs.state===S_WAITING # because not set
    @test bs.f===bf
    @test bs.sign===0.0
    @test bs.strange===ONE
    @test bs.ac === 'b'
    @test bs.first === ba1
    @test bs.second === BSArg() # because not set
    
    # verify a lot of field settings
    vecsize = 1000
    rng = MersenneTwister(1234);
    uv = Vector{UInt64}(undef,vecsize)
    rand!(rng,uv)
    for u in uv
        bu = reinterpret(BS,u)
        bu2 = BS()
        for s in fieldnames(BS)
            bu2 = setproperty(bu2,s,getproperty(bu,s))
            if getproperty(bu2,s) !== getproperty(bu,s)
                @test getproperty(bu2,s) === getproperty(bu,s)
            end
        end
    end
end


@testset "BitStruct read properties" begin
    testReadProperties()

end

