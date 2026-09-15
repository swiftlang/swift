// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// Test that tuples containing multiple pack expansions can be passed to
// functions expecting a single pack expansion tuple parameter.
// This tests the fix for the SILGen assertion "only single pack expansion
// tuples are currently supported" when combining two packs into one tuple.

protocol P {}
struct A: P {}

struct Holder<each T: P> {
    let values: (repeat each T)
}

// Test 1: Direct inline construction of multi-pack tuple
// CHECK-LABEL: sil hidden [ossa] @$s26pack_expansion_multi_tuple10testInline
func testInline<each X: P, each Y: P>(_ x: (repeat each X), _ y: (repeat each Y)) {
    // This creates a tuple (repeat each X, repeat each Y) with two pack expansions
    // and passes it to Holder which expects (repeat each T)
    let h = Holder(values: (repeat each x, repeat each y))
    print(h)
}

// Test 2: Store multi-pack tuple in variable first, then pass it
// This tests the case where the tuple is an RValue, not an inline expression
// CHECK-LABEL: sil hidden [ossa] @$s26pack_expansion_multi_tuple10testStored
func testStored<each X: P, each Y: P>(_ x: (repeat each X), _ y: (repeat each Y)) {
    let combined: (repeat each X, repeat each Y) = (repeat each x, repeat each y)
    let h = Holder(values: combined)
    print(h)
}

// Test 3: Multi-pack tuple returned from helper function
// CHECK-LABEL: sil hidden [ossa] @$s26pack_expansion_multi_tuple6concat
func concat<each X: P, each Y: P>(
    _ x: (repeat each X),
    _ y: (repeat each Y)
) -> (repeat each X, repeat each Y) {
    return (repeat each x, repeat each y)
}

// CHECK-LABEL: sil hidden [ossa] @$s26pack_expansion_multi_tuple14testFromHelper
func testFromHelper<each X: P, each Y: P>(_ x: (repeat each X), _ y: (repeat each Y)) {
    let combined = concat(x, y)
    let h = Holder(values: combined)
    print(h)
}

func callTests() {
    testInline((A(),), (A(),))
    testStored((A(),), (A(),))
    testFromHelper((A(),), (A(),))
}
