// RUN: %swift -i -constraint-checker %s

// Check to make sure we are actually getting Optionals out of this
// Generator
var w = asGenerator((1..2).getEnumeratorType())
var maybe_one = w.next()
assert(maybe_one != None)
for one in maybe_one {
  assert(one == 1)
}
assert(w.next() == None)

// Test round-trip Generator/Enumerator adaptation
var x = asGenerator((1..7).getEnumeratorType())
var y = asEnumerator(x)
var z = ZipEnumerator2(y, (1..7).getEnumeratorType())

var a : (Int,Int)
while !z.isEmpty() {
  a = z.next()
  assert(a.0 == a.1)
}

assert(a.0 == 6)
println("done.")
