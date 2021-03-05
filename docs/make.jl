using BitStructs
using Documenter

DocMeta.setdocmeta!(BitStructs, :DocTestSetup, :(using BitStructs); recursive=true)

makedocs(;
    modules=[BitStructs],
    authors="Robert Rudolph",
    repo="https://github.com/rryi/BitStructs.jl/blob/{commit}{path}#{line}",
    sitename="BitStructs.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://rryi.github.io/BitStructs.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/rryi/BitStructs.jl",
)
