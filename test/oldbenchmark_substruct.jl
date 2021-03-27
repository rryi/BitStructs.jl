using BitStructs
using Random
using BenchmarkTools

include("types_in_tests.jl")
#=
module BitStructs
function bitsizeof end
function encode end
function decode end
end
=#

function prompt(p::AbstractString="")
    println(p, " ENTER: ")
    #return chomp(readline())
end



const vecsize = 100 # vector size for benchmark loop
rng = MersenneTwister(1234);
uv = Vector{UInt64}(undef,vecsize)
rand!(rng,uv)
bsv = BS[]
isv = IS[]
msv = MS[]

#bs = reinterpret(BS,uv[1])
#is = fieldcopy(IS,bs)

for i = 1:vecsize
    local bs = reinterpret(BS,uv[i])
    push!(bsv,bs)
    local is = fieldcopy(IS,bs)
    push!(isv,is)
    local ms = fieldcopy(MS,bs)
    push!(msv,ms)
end



prompt("foo flag or: test with || if any flag is set in loop mutable struct/struct/BitStruct/ /=")
function foo(b::Vector{T}) where T
    s = 0
    for st in b
        if st.f.f1 || st.f.f2 || st.f.f3
            s += 1
        end
    end        
    return s
end
@btime (foo($msv))
@btime (foo($isv))
@btime (foo($bsv))

prompt("fo flag or: test with | if any flag is set in loop mutable struct/struct/BitStruct/ /=")
function fo(b::Vector{T}) where T
    cmp = b[1].f
    s = 0
    for st in b
        if st.f !== cmp
            s += 1
        end
    end        
    return s
end
@btime (fo($msv))
@btime (fo($isv))
@btime (fo($bsv))

prompt("Constructors without argument are fast")
@btime BS()
@btime BFlags()
@btime BSArg()

prompt("Constructors with arguments are terrible slow - field assignment by set or / is 1000* faster")
@btime BFlags(false,true,false)
@btime (BFlags() / (:f1,false) / (:f2,true) / (:f3,false)) 




prompt("w0 write subfields in loop mutable struct/struct/BitStruct")
@noinline function w0(b::Vector{T}) where T
    s = 0%UInt64
    for i in one(UInt64):UInt64(100)
        b[i] = setproperty(b[i],:first, setproperty(b[i].first,:id,b[i].second.id))
    end        
    return b
end
@btime (w0($msv))
@btime (w0($isv))
@btime (w0($bsv))

prompt("b0 read subfields in loop mutable struct/struct/BitStruct")
@noinline function b0(b::Vector{T}) where T
    s = 0%UInt64
    for i in one(UInt64):UInt64(100)
        s += b[i].first.id + b[i].second.id
    end        
    return s
end
@btime (b0($msv))
@btime (b0($isv))
@btime (b0($bsv))

## method param benchmark


@inline function work(v1::Vector{Int}, v2::Vector{Int}, s) # s is a ?S
    v1[1] = v2[1] # just to force use of v1,v2 
    if (s.f.f1 || s.f.f2) && (s.first.f.f3 || s.second.f.f3)
        s = setproperty(s,:bit,-1)
        s = setproperty(s,:first, s.second)
    else
        s = setproperty(s,:bit,0)
        s = setproperty(s,:second, s.first)
    end
    v2[2] = v1[2]
    s = setproperty(s,:sign,Float64(v1[3]-v2[3]))
    return subWork(v1,v2,s.first)+subWork(v2,v1,s.second)
end

@inline function subWork(v1::Vector{Int}, v2::Vector{Int}, s) # s is a ?SArg
    v2[1] = v1[3] # just to force use of v1,v2 
    if s.f.f1
        s = setproperty(s, :id, (abs(s.delta)+v2[3])%UInt )
    else
        s = setproperty(s, :delta, s.id/2 -v2[3])
    end
    if s.state == S_RUNNING 
        s = setproperty(s, :state, S_DONE)
    else
        s = setproperty(s, :state, S_FAILURE)
    end
    return s.id+s.delta
end


v1 = [1,2,3]
v2 = [4,5,6]

prompt("function parameter test mutable struct/struct/BitStruct")
@btime work($v1,$v2,$msv[1])
@btime work($v1,$v2,$isv[1])
@btime work($v1,$v2,$bsv[1])


