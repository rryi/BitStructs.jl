# BitStructs.jl

Julia struct-s packed at bit boundaries into a primitive 64 bit type.

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://rryi.github.io/PackedStructs.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://rryi.github.io/PackedStructs.jl/dev)
[![Build Status](https://travis-ci.com/rryi/PackedStructs.jl.svg?branch=master)](https://travis-ci.com/rryi/PackedStructs.jl)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/rryi/PackedStructs.jl?svg=true)](https://ci.appveyor.com/project/rryi/PackedStructs-jl)
[![Build Status](https://api.cirrus-ci.com/github/rryi/PackedStructs.jl.svg)](https://cirrus-ci.com/github/rryi/PackedStructs.jl)
[![Coverage](https://codecov.io/gh/rryi/PackedStructs.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/rryi/PackedStructs.jl)

BitStructs has the following use cases in mind: 

 * memory reduction for tables with fields having very few instances like flags, status info, enumerations. A couple of columns in such tables could be combined into a Vector{T<:BitStruct}
 
 * pooling of several method parameters in one BitStruct instance. This reduces push/pop overhead in method execution for methods which are not inlined, and can improve runtime performance, because the compiler gets more choices for keeping data in CPU registers.

 * reading and writing of bitpacked binary data 

 # current state: in development

 As soon as I think the package is usable, it will become a registered julia package.
 

 # syntax

 # reading a BitStruct field

 # writing a BitStruct field

Well, BitStruct is an immutable type. Literally writing a field is not possible.
But we can construct a new BitStruct having a field replaced. To have a neat short syntax,
the division operator is overloaded: left side is a BitStruct, right side a Tuple{Symbol,T} 
where T is the type of the BitStruct field. An Example:


    @bitstruct BS begin
    i1 :: BInt{6}
    u1 :: UInt8
    end

    bs = BS(i1=3,u1=0x02)

    bs = bs / (:u1,0x17) # replaces field :u1 value by 0x17

    bs /= :i1,5  # assigns 5 to bitfield :i1

TODO: support |= &= as well? NO 