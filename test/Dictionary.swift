// RUN: %swift %s -i

var d = new Dictionary(13)
d["one"] = 1
d["two"] = 2
d["three"] = 3
d["four"] = 4
d["five"] = 5
assert(d["one"] == 1)
assert(d["two"] == 2)
assert(d["three"] == 3)
assert(d["four"] == 4)
assert(d["five"] == 5)

// Iterate over (key, value) tuples as a silly copy
var d2 = new Dictionary(13)
for (key, value) in d {
  d2[key] = value
}

assert(d2["one"] == 1)
assert(d2["two"] == 2)
assert(d2["three"] == 3)
assert(d2["four"] == 4)
assert(d2["five"] == 5)

