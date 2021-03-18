using BitStructs

# abstract example
module M

function bitsizeof(::Type{T}) where T<: Enum 
    # use "new world" in all function calls depending on T
    8*sizeof(Int) - leading_zeros(Int(Base.invokelatest(typemax,T))-Int(Base.invokelatest(typemin,T)))
end

fib(i::UInt) = i<=1 ? 1 : fib(i-1)+fib(i-2) # I know you can optimize it - compiler can't
transform(s) = fib(hash(string(s))%48) # just for demo: a pure but expensive function

function f0(::Type{T},s) where {T<:Enum}
    bits = bitsizeof(T)
    sTransformed = transform(s) # expensive pure calculation
    return (T,bits,sTransformed)
end

@generated function f1(::Type{T},::Val{s}) where {T<:Enum , s}
    bits = Int(Base.invokelatest(bitsizeof,T)) 
    sTransformed = transform(s) # expensive pure calculation
    return :(($T,$bits,sTransformed))
end

function f2 end

function generate(::Type{T}) where {T<:Enum}
    bits = bitsizeof(T)
    for s in instances(T) # generate method per enum instance
        sTransformed = transform(s) # expensive pure calculation
        ex = :(function f2(::Type{T}, ::Val{$s}) where {T <: Enum}
        return (T, $bits, $sTransformed)
        end)
        eval(ex)
    end
    return nothing
end
export f0,f1,f2,generate

end # module M

module App
using Main.M
@enum MyEnum ::Int8 e1 = -5 e2 = 0 e3=5
export MyEnum, e1,e2,e3
generate(MyEnum)
end # module app

# in application (another module)
using Main.App
using Main.M


f0(App.MyEnum,e2)
f2(App.MyEnum,Val(e2))
@time f0(App.MyEnum,e2)
@time f2(App.MyEnum,Val(e2))
f1(App.MyEnum,Val(e2))





NT = @NamedTuple{ f1::Bool, f2:: BUInt{1}, i1::BUInt{6}, i2::BInt{8}, i3::UInt8, i4::Int8, u16::UInt16, i16::Int16}
const BS = BitStruct{NT}

bs = BS(;i1=5,i3=8)


#= isdefined to check on world age problem

function CreateMatrix(Ncount;Plot=true)
    TheMatrix = fill(0.0,Ncount,Ncount)

    if Plot
        if isdefined(Main, :PyPlot)
            println("PyPlot already loaded")
            PyPlot.figure()
            PyPlot.imshow(abs.(TheMatrix))
            PyPlot.colorbar()
        else
            println("PyPlot loading PyPlot")
            @eval using PyPlot
            Base.invokelatest(PyPlot.figure)
            Base.invokelatest(PyPlot.imshow, abs.(TheMatrix))
            Base.invokelatest(PyPlot.colorbar)
        end
    end
    return TheMatrix
end
=#


s="""
function _fielddescr1(::Type{BitStruct{T}},::Val{s}) where {T<:NamedTuple, s}
    shift = 0
    types = T.parameters[2].parameters
    syms = T.parameters[1]
    idx = 1
    while idx <= length(syms)
        type = types[idx]
        bits = bitsizeof(type)
        if syms[idx]===s
            return type,shift,bits
        end
        shift += bits
        idx += 1
    end
    throw(ArgumentError(s))
end
"""
Meta.parse(s)


s=
"""function f(::Type{T},::Val{s}) where {T<:Enum , s}
    sTransformed = s # just for demo. In real application, a time consuming but pure calculation
    bits = Int(Base.invokelatest(bitsizeof,T)) 
    return (T,bits,sTransformed)
end"""

s=
"""f(::Type{T},::Val{s}) where {T<:Enum , s} = (T,:bits,:sTransformed)"""


s=
"""
@inline function Base.getproperty(x::BitStruct{T},s::Symbol) where T<:NamedTuple
    type,shift,bits = 1,2,3
    return _convert(type,_get(reinterpret(UInt64,x),shift,bits))
end"""


type=1
shift = 2
bits = 3

s="@inline Base.getproperty(x::BitStruct{T},Val{s}) where {T<:NamedTuple, s} = _convert($type,_get(reinterpret(UInt64,x),$shift,$bits))"

julia> Meta.parse(s)

:(#= none:1 =# @inline (Base.getproperty(x::BitStruct{T}, Val{s}) where {T <: NamedTuple, s}) = begin
              #= none:1 =#
              _convert(1, _get(reinterpret(UInt64, x), 2, 3))
          end)



end



macro m3(ex1,ex2,ex3)
    show(ex1)
    show(ex2)
    show(ex3)
end

# führt ex1 ex2 im caller kontext aus, also @m2 i=5 j=6 macht zwei Zuweisungen auf i,j
macro m2(ex1,ex2)
    println()
    show(ex1)
    println()
    show(ex2)
    println()
    ret = quote
        $(esc(ex1))
        $(esc(ex2))
    end
    return ret
end


@m3 const BS = BitStruct{NT};function get(x::BS) x+5 end;  function put(x::BS) x+7 end;



@m2 myname BitStruct{NT}

:myname
:(BitStruct{NT})


@m2 const BS = BitStruct{NT}
get(x::BS) = x+5



julia> @m2 BS  BitStruct{NT}

:BS
:(BitStruct{NT})



@m2 bs begin
    a=b
end



# das setzt variable ex1 auf ergebnis Ausdruck ex2
macro m4(ex1, ex2)
    ret = quote
        const $(esc(ex1)) = $(esc(ex2))
    end
    show(ret)
    return ret
end


# definiert const und legt fkt mit Val typeparameter an.
macro m5(ex1::Symbol, ex2::Expr)
    fctsym = Symbol("f")
    ret = quote
        const $(esc(ex1)) = $(esc(ex2))
        function $(esc(fctsym))(::Val{$(esc(ex1))}) 
            $(esc(ex2))
        end
    end
    show(ret)
    println()
    return ret
end

macro m1(ex)
    show(ex)
    println()
end


#aus forum, working:
macro customFun(vectype::Expr, name::Symbol)
    quote
        function $(esc(name))(v::$(esc(vectype)))
            println(typeof(v))
        end
    end
end
#=
julia> @customFun Vector{Int} f
f (generic function with 1 method)

julia> f(Int[])
Array{Int64,1}
=#




@bs mybs begin
    flag4 :: Bool
    ascii1 :: BUInt{7} # some comment
    v1 :: UInt16 
end


function m(; kwargs...)
        dump(kwargs)
        println(kwargs.data)
        println(typeof(kwargs))
        println(kwargs[1])
        println(typeof(kwargs[1]))
        return kwargs.data
       end