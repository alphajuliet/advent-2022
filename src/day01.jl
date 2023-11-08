### A Pluto.jl notebook ###
# v0.19.32

using Markdown
using InteractiveUtils

# ╔═╡ 3af37f15-2462-4b40-a045-2907a8e132b6
using Lazy, Pipe, Markdown

# ╔═╡ 2a7ca908-c210-4ee6-8d01-14524bb4596a
md"""
# Advent of Code 2022 - Day 01
"""

# ╔═╡ f8be88f8-2d19-44f4-b21e-c0d92985a0db
begin
	test_file = "../data/day01-test.txt"
	input_file = "../data/day01-input.txt"
end;

# ╔═╡ 751d762d-8bbc-4755-824e-692c93aee70d
"Parser to integers that can handle empty strings"
function parse_int(str :: AbstractString)
	str != "" || return nothing
	return parse(Int, str)
end

# ╔═╡ 7cf79487-fcc2-4516-bb2c-4676b696a48c
function read_sums(fname)
	@pipe fname |>
		readlines |>
		map(parse_int, _) |> 
		Lazy.split(_, nothing) |>
		map(sum, _)
end;

# ╔═╡ e6ae5e85-1c85-4af3-b2a8-33b367f75583
function part1(fname)
	@pipe fname |>
		read_sums |>
		maximum
end;

# ╔═╡ 44f4c520-979a-45f3-b7de-e28bce4efdc8
part1(test_file)

# ╔═╡ 97321a02-27e4-4037-8b83-51d28472005a
part1(input_file)

# ╔═╡ e7ca0c4a-3718-43a9-a296-3d5b8486bb21
function part2(fname)
	@pipe fname |>
		read_sums |>
		sort(_, lt=(>)) |>
		Lazy.take(_, 3) |>
		collect |>
		sum
end;

# ╔═╡ 0dbb0400-d7d0-40c7-a0d3-d4989db50ac9
part2(test_file)

# ╔═╡ d24afbc7-5b12-4bd1-b037-2eba676334ac
part2(input_file)

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
Lazy = "50d2b5c4-7a5e-59d5-8109-a42b560f39c0"
Markdown = "d6f4376e-aef5-505a-96c1-9c027394607a"
Pipe = "b98c9c47-44ae-5843-9183-064241ee97a0"

[compat]
Lazy = "~0.15.1"
Pipe = "~1.3.0"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.9.3"
manifest_format = "2.0"
project_hash = "089b53e7aa8f2afa97b2f742ed4ba5d5528d8c94"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

[[deps.Lazy]]
deps = ["MacroTools"]
git-tree-sha1 = "1370f8202dac30758f3c345f9909b97f53d87d3f"
uuid = "50d2b5c4-7a5e-59d5-8109-a42b560f39c0"
version = "0.15.1"

[[deps.MacroTools]]
deps = ["Markdown", "Random"]
git-tree-sha1 = "9ee1618cbf5240e6d4e0371d6f24065083f60c48"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.11"

[[deps.Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[deps.Pipe]]
git-tree-sha1 = "6842804e7867b115ca9de748a0cf6b364523c16d"
uuid = "b98c9c47-44ae-5843-9183-064241ee97a0"
version = "1.3.0"

[[deps.Random]]
deps = ["SHA", "Serialization"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"
version = "0.7.0"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"
"""

# ╔═╡ Cell order:
# ╟─2a7ca908-c210-4ee6-8d01-14524bb4596a
# ╠═3af37f15-2462-4b40-a045-2907a8e132b6
# ╠═f8be88f8-2d19-44f4-b21e-c0d92985a0db
# ╠═751d762d-8bbc-4755-824e-692c93aee70d
# ╠═7cf79487-fcc2-4516-bb2c-4676b696a48c
# ╠═e6ae5e85-1c85-4af3-b2a8-33b367f75583
# ╠═44f4c520-979a-45f3-b7de-e28bce4efdc8
# ╠═97321a02-27e4-4037-8b83-51d28472005a
# ╠═e7ca0c4a-3718-43a9-a296-3d5b8486bb21
# ╠═0dbb0400-d7d0-40c7-a0d3-d4989db50ac9
# ╠═d24afbc7-5b12-4bd1-b037-2eba676334ac
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
