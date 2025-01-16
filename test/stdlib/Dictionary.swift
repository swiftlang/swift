// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var DictionaryTests = TestSuite("Dictionary")

// Test: Remove a key-value pair that satisfies the condition
DictionaryTests.test("RemoveWhere_ElementRemoved") {
    var dict: [Int: String] = [1: "One", 2: "Two", 3: "Three", 4: "Four", 5: "Five"]
    
    // Remove the first key-value pair where the value length is greater than 3
    let removed = dict.remove(where: { $0.value.count > 3 })
    expectEqual(removed?.key, 4)
    expectEqual(removed?.value, "Four")
    
    // Verify the dictionary after removal
    expectNil(dict[4])
}

// Test: No key-value pair satisfies the condition
DictionaryTests.test("RemoveWhere_NoElementRemoved") {
    var dict: [Int: String] = [1: "One", 2: "Two", 3: "Three", 4: "Four", 5: "Five"]
    
    // Try to remove a key-value pair where the value's length is greater than 10
    let removed = dict.remove(where: { $0.value.count > 10 })
    
    expectNil(removed)
}

// Test: Removing a key-value pair from an empty dictionary
DictionaryTests.test("RemoveWhere_EmptyDictionary") {
    var dict: [Int: String] = [:]
    
    // Try to remove a key-value pair from an empty dictionary
    let removed = dict.remove(where: { $0.key > 3 })
    
    expectNil(removed)
}

// Test: Multiple key-value pairs satisfy the condition
DictionaryTests.test("RemoveWhere_MultipleElements") {
    var dict: [Int: String] = [1: "One", 2: "Two", 3: "Three", 4: "Four", 5: "Five"]
    
    // Remove the first key-value pair where the key is greater than 2
    let removed = dict.remove(where: { $0.key > 2 })
    expectEqual(removed?.key, 3)
    expectEqual(removed?.value, "Three")
}

// Execute the test suite
runAllTests()
