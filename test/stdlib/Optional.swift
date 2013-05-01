// RUN: %swift -i -constraint-checker %s

var x = Optional<Int>()
if x { assert(false, "x is supposed to be false") }
else { println("an empty optional is logically false") }
assert(x == None)
assert(None == x)

for y in x {
    assert(false, "x is supposed to be empty")
}

x = Optional<Int>(0)

x = Some(1)

if x { println("an non-empty optional is logically true") }
else { assert(false, "x is supposed to be true") }
assert(x != None)
assert(None != x)

if !x { assert(false, "x is supposed to be true") }
else { println("logical negation works") }

var empty: Bool = true
for y in x {
    assert(y == 1)
    empty = false
    println("destructuring bind works")
}
assert(!empty)

x = +None
assert(!x)
