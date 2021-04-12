# abstract example
module M

function bitsizeof(::Type{T}) where T<: Enum 
    # use "new world" in all function calls depending on T
    8*sizeof(Int) - leading_zeros(Int(Base.invokelatest(typemax,T))-Int(Base.invokelatest(typemin,T)))
end

function bitsizeof(::Type{T}) where T<: Integer 
    # use "new world" in all function calls depending on T
    8*sizeof(T)
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


function shownames()
    println("names(Main) = ", names(Main))
    println("names(Main.M) = ", names(Main.M))
    println("names(Main.App) = ", names(Main.App))
    t = Symbol("MyEnum")
    println("typeof(Symbol(MyEnum)) = ",typeof(t))
    try
        println("eval(Symbol(MyEnum) = ",eval(t))
    catch
        println("eval(Symbol(MyEnum) is undefined")
    end
    t = Symbol("Main.App.MyEnum")
    println("typeof(Symbol(Main.App.MyEnum)) = ",typeof(t))
    try
        println("eval(Symbol(Main.App.MyEnum)) = ",eval(t))
    catch
        println("eval(Symbol(Main.App.MyEnum)) is undefined")
    end
end



macro bitstruct(name,ex)
    Meta.isexpr(ex, :braces) || Meta.isexpr(ex, :block) ||
        throw(ArgumentError("@bitstruct expects name {...} or name begin...end"))

    decls = filter(e -> !(e isa LineNumberNode), ex.args)
    all(e -> Meta.isexpr(e, :(::)), decls) || throw(ArgumentError("@bitstruct must contain a sequence of name::type expressions, nothing else"))
    for e in decls 
        #println("ex item: ",e," :: ",typeof(e), " -> args",e.args)        
    end
    vars = [QuoteNode(e.args[1]) for e in decls]
    types = [esc(e.args[2]) for e in decls]
    bitstruct = :(BitStruct{NamedTuple{($(vars...),), Tuple{$(types...)}}})
    println("bitstruct type is: ",bitstruct) 
    # so far adopted from @NamedTuple. Now: build fielddescr table
    shownames()

    fieldsyms = Symbol[]
    fielddscrs = Tuple{DataType,Int,Int}[] # type, shift, bits
    shift = 0
    for e in decls 
        println("field name = ",e.args[1], ", bitfield type = ", e.args[2], ", typeof(bitfield type) = ",typeof(e.args[2]))
        t = e.args[2] # type
        println("t = ",t)
        t = eval(t)
        println("t=eval(t) = ",t)
        println("typeof(t) = ",typeof(t))
        println("dump(t) = ",dump(t)) # error in this line: MyEnum not defined
        bits = bitsizeof(t)
        push!(fieldsyms,e.args[1])
        push!(fielddscrs,(t,shift,bits))
        shift += bits
        #println("shift=",shift)
        shift > 64 && throw(DomainError(e.args[1],"BitStruct too huge: would exceed a total bitsize of 64"))
    end
    # build the expressions to execute
    ret = quote
        const $(esc(name)) = $(bitstruct)
    end
    return ret
end

export f0,f1,f2,generate, @bitstruct, shownames

end # module M

module App
@enum BeforeUsingM ::Int8 b1 = -5 b2 = 0 b3=5
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
#@time f0(App.MyEnum,e2)
#@time f2(App.MyEnum,Val(e2))
#f1(App.MyEnum,Val(e2))

shownames()

@bitstruct TestBitStruct begin
    field1::Int8
    field2::MyEnum  
end


