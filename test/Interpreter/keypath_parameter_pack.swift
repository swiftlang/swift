// RUN: %target-run-simple-swift(-target %target-swift-5.9-abi-triple)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ParameterPacks

// Test that key paths on types containing parameter packs work at runtime
// for both concrete type specializations and abstract pack contexts.

struct Entry<each T> {
    let input: (repeat each T)
    let name: String
}

// Container that uses keypaths on pack types in a generic context
struct Container<each T> {
    let entries: [Entry<repeat each T>]

    // Keypath to pack tuple property - tests abstract pack keypath
    var inputs: [(repeat each T)] {
        entries.map(\.input)
    }

    // Keypath to non-pack property for comparison
    var names: [String] {
        entries.map(\.name)
    }
}

// Test direct keypath usage with concrete type specialization
func testDirectKeyPath() {
    // Single-element pack case
    let entry1 = Entry<Int>(input: 42, name: "e1")
    let keyPath1 = \Entry<Int>.input
    let value1 = entry1[keyPath: keyPath1]
    assert(value1 == 42)
    print("testDirectKeyPath single: PASS")

    // Two-element pack case
    let entry2 = Entry<Int, String>(input: (42, "hello"), name: "e2")
    let keyPath2 = \Entry<Int, String>.input
    let value2 = entry2[keyPath: keyPath2]
    assert(value2.0 == 42)
    assert(value2.1 == "hello")
    print("testDirectKeyPath double: PASS")

    // Three-element pack case
    let entry3 = Entry<Int, String, Double>(input: (42, "hello", 3.14), name: "e3")
    let keyPath3 = \Entry<Int, String, Double>.input
    let value3 = entry3[keyPath: keyPath3]
    assert(value3.0 == 42)
    assert(value3.1 == "hello")
    assert(value3.2 == 3.14)
    print("testDirectKeyPath triple: PASS")
}

// Test keypaths used inside generic contexts with abstract packs
func testAbstractPackKeyPath() {
    // Test with single-element pack
    let container1 = Container(entries: [
        Entry<Int>(input: 1, name: "first"),
        Entry<Int>(input: 2, name: "second")
    ])
    let inputs1 = container1.inputs
    assert(inputs1.count == 2)
    assert(inputs1[0] == 1)
    assert(inputs1[1] == 2)
    let names1 = container1.names
    assert(names1[0] == "first")
    assert(names1[1] == "second")
    print("testAbstractPackKeyPath single: PASS")

    // Test with two-element pack
    let container2 = Container(entries: [
        Entry<Int, String>(input: (1, "a"), name: "entry1"),
        Entry<Int, String>(input: (2, "b"), name: "entry2")
    ])
    let inputs2 = container2.inputs
    assert(inputs2.count == 2)
    assert(inputs2[0].0 == 1)
    assert(inputs2[0].1 == "a")
    assert(inputs2[1].0 == 2)
    assert(inputs2[1].1 == "b")
    print("testAbstractPackKeyPath double: PASS")
}

testDirectKeyPath()
testAbstractPackKeyPath()
print("All tests passed!")
