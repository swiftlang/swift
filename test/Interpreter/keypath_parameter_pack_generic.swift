// RUN: %target-run-simple-swift(-target %target-swift-5.9-abi-triple)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ParameterPacks

// Test that key paths on types containing parameter packs work at runtime
// in generic contexts where the pack is not yet concretely specialized.

struct Entry<each T> {
    let input: (repeat each T)
}

struct Container<each T> {
    let entries: [Entry<repeat each T>]

    var inputs: [(repeat each T)] {
        entries.map(\.input)
    }
}

// Test with single type parameter
func testSingleParam() {
    let entries = [
        Entry<Int>(input: 42),
        Entry<Int>(input: 17)
    ]
    let container = Container(entries: entries)
    let inputs = container.inputs

    assert(inputs.count == 2)
    assert(inputs[0] == 42)
    assert(inputs[1] == 17)
    print("testSingleParam: PASS")
}

// Test with multiple type parameters
func testMultipleParams() {
    let entries = [
        Entry<Int, String>(input: (1, "one")),
        Entry<Int, String>(input: (2, "two"))
    ]
    let container = Container(entries: entries)
    let inputs = container.inputs

    assert(inputs.count == 2)
    assert(inputs[0] == (1, "one"))
    assert(inputs[1] == (2, "two"))
    print("testMultipleParams: PASS")
}

// Test with three type parameters
func testTripleParams() {
    let entries = [
        Entry<Int, String, Double>(input: (1, "one", 1.0)),
        Entry<Int, String, Double>(input: (2, "two", 2.0))
    ]
    let container = Container(entries: entries)
    let inputs = container.inputs

    assert(inputs.count == 2)
    assert(inputs[0] == (1, "one", 1.0))
    assert(inputs[1] == (2, "two", 2.0))
    print("testTripleParams: PASS")
}

testSingleParam()
testMultipleParams()
testTripleParams()
print("All tests passed!")
