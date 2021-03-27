
function testConstructors()
    local bf0
    @test (bf0 = BFlags()) isa BFlags
    @test bf0 isa BitStruct
    @test BFlags <: BitStruct

    local ba0
    @test (ba0 = BSArg()) isa BSArg    
    local bs
    @test (bs0 = BS()) isa BS
    local bf1
    # incomplete param list will initialize the rest with 0
    @test (bf1 = BFlags(true)) isa BFlags
    @test (bf100 = BFlags(true,false,false)) isa BFlags
    # onstructor calls
    @test_throws MethodError  BFlags(true,1,2)
    @test_throws BoundsError  BFlags(true,false,false,false)
    @test (bf1 = BFlags(true)) isa BFlags

    @test (bs0 = BS(BIGMINUS,-1.0,bf1,S_RUNNING,-1,'a',ba0,ba0)) isa BS
end


@testset "BitStruct constructors" begin
    testConstructors()
end

