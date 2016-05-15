# Changelog parsing

`Changes.of_string` and `Changes.of_channel` parse an OCaml community
style changelog and produce a `(Changes.t, string)
result` which can be destructed for the structured changelog or the
parse error if one occurred. `Changes.to_string` serializes a `Changes.t`.
