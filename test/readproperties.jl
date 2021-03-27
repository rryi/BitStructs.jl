
function testReadProperties()
    bf = BFlags()
    @test (bf.f1 | bf.f1 | bf.f3) == false
    bs = BS()
    @test reinterpret(UInt64,bs) === zero(UInt64)
    @test bf === bs.f

    bf = BFlags(true,false,true)
    @test bf.f1 
    @test bf.f3
    @test !bf.f2
    @test_throws MethodError bf.f4 


    ba1 = BSArg(bf,S_DONE,7%UInt64,-7)
    @test ba1.id + ba1.delta === zero(UInt64)

    ba2 = BSArg(BFlags(),S_FAILURE,1023%UInt64,-256)
    @test ba2.id + ba2.delta === UInt64(1023-256)

    bs1 = BS(BIGMINUS,-1.0,bf,S_RUNNING,-1,'a',ba1,ba2)
    @test bs1.strange === BIGMINUS
    @test bs1.sign === -1.0
    @test bs1.state === S_RUNNING
    @test bs1.ac === 'a'
    @test bs1.first === ba1
    
    bs2 = BS(BIGMINUS,-1.0,bf,S_RUNNING,-1,'a',ba2,ba1)
    @test bs1 !== bs2
    @test bs1 != bs2
    
    bs1again = BS(BIGMINUS,-1.0,bf,S_RUNNING,-1,'a',ba1,ba2)
    @test bs1 == bs1again
    @test bs1 === bs1again
end


@testset "BitStruct read properties" begin
    testReadProperties()

end

