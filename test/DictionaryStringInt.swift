// RUN: %swift %s -i

var d : DictionaryStringInt
d.setBucketCount(13)
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
