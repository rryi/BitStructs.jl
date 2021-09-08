using Documenter
using BitStructs

push!(LOAD_PATH,"../src/")
makedocs(
    sitename = "BitStructs",
         pages = [
            "Index" => "index.md",
            "Tutorial" => "tutorial.md",
            "Benchmarks" => "benchmarks.md",
         ],
    format = Documenter.HTML(),
    modules = [BitStructs]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
deploydocs(
    repo = "github.com/rryi/BitStructs.jl.git",
    devbranch = "main"
)
