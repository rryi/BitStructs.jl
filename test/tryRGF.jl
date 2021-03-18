module M
using RuntimeGeneratedFunctions
RuntimeGeneratedFunctions.init(@__MODULE__)

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




