// RUN: %swift -i -constraint-checker %s
var n = [2, 3, 5, 7, 11]
var s = ["two", "three", "five", "seven", "eleven", "thirteen"]

var i = 0
for p in Zip2(n, s) {
    println("\(p.0)\t\(p.1)")
    assert( p.0 == n[i] )
    assert( p.1 == s[i] )
    ++i
}

assert(i == n.length)

i = 0
for p in Zip2(s, n) {
    println("\(p.0)\t\(p.1)")
    assert( p.1 == n[i] )
    assert( p.0 == s[i] )
    ++i
}
assert(i == n.length)

println("done.")

