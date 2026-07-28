// RUN: %target-swift-frontend -g -O -emit-ir %s -o /dev/null

// alloc_pack simplification must produce well-typed debug_value when a
// single-element pack is eliminated, rather than assert

@inline(__always)
func scrub<each T>(_ items: repeat each T) -> Int {
    var tally = 0
    func increment<U>(_ u: U) {
        tally += 1
    }
    repeat increment(each items)
    return tally
}

@inline(__always)
func generate<Value>(_ seed: Value) -> Int {
    return scrub(seed)
}

let x = generate(42)
