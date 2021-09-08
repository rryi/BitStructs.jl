# BitStructs.jl

Julia struct-s packed at bit boundaries into a primitive 64 bit type.

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://rryi.github.io/BitStructs.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://rryi.github.io/BitStructs.jl/dev)
[![Build Status](https://travis-ci.com/rryi/BitStructs.jl.svg?branch=master)](https://app.travis-ci.com/rryi/BitStructs.jl)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/rryi/BitStructs.jl?svg=true)](https://ci.appveyor.com/project/rryi/BitStructs.jl)
[![Build Status](https://api.cirrus-ci.com/github/rryi/BitStructs.jl.svg)](https://cirrus-ci.com/github/rryi/BitStructs.jl)
[![Coverage](https://codecov.io/gh/rryi/BitStructs.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/rryi/BitStructs.jl)

BitStructs has the following use cases in mind: 

 * memory reduction for tables with fields having very few instances like flags, status info, enumerations. A couple of columns in such tables could be combined into a Vector{T<:BitStruct}
 
 * pooling of several method parameters in one BitStruct instance. This reduces push/pop overhead in method execution for methods which are not inlined, and can improve runtime performance, because the compiler gets more choices for keeping data in CPU registers.

 * reading and writing of bitpacked binary data 

 # current state: in development

 As soon as I think the package is usable, it will become a registered julia package.

see test/tutorial.jl for an introduction

see test/benchmarks.jl for some timing comparisons

With julia 1.6-RC1, reading fields is nearly as fast as reading fields in standard julia struct-s. 
Older julia releases do not perform full constant propagation on field read access ==> 100-1000 times slower.

Setting fields is currently under investigation.

