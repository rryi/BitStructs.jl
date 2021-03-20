#@nospecialize 
function f(a,b) 
    println("nospecialize on f($a,$b)")
end

methods(f)
f(1,1)
f(a::Int,b::Int) = println("specialize on concrete types f(Int,Int)")

f(a::Float64,b::U) where U = println("specialize on f(Float,U) where U")

methods(f)
f(1,1)
f(true,1)
f(2.0,true)
f(0x1,1.0)
f(1.0,0x1)
methods(f)
