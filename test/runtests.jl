using BitStructs
using Test
using Random

include("types_in_tests.jl")

@testset "BitStructs.jl" begin
    include("constructors.jl")
    include("readproperties.jl")
    include("writeproperties.jl")
    #include("substructs.jl")
    #include("io.jl")
    # Write your tests here.
end
