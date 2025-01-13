// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import XCTest

// Assuming the Dictionary.swift test file is using XCTest
class DictionaryTests: XCTestCase {

    // Existing test case, as you provided
    func dictionaryAlgebraOps<K: Hashable, V>(_ dict: Dictionary<K, V>) {}

    func testDictionaryAlgebraOperations() {
        dictionaryAlgebraOps([1: "One", 2: "Two", 3: "Three"])
    }

    // Your new test cases for remove(where:)

    // Test: Remove a key-value pair that satisfies the condition
    func testRemoveWhere_ElementRemoved() {
        var dict: [Int: String] = [1: "One", 2: "Two", 3: "Three", 4: "Four", 5: "Five"]
        
        // Remove the first key-value pair where the value length is greater than 3
        if let removed = dict.remove(where: { $0.value.count > 3 }) {
            XCTAssertEqual(removed.key, 4, "The removed key should be 4.")
            XCTAssertEqual(removed.value, "Four", "The removed value should be 'Four'.")
        } else {
            XCTFail("Expected to remove an element, but none was removed.")
        }
        
        // Verify the dictionary after removal
        XCTAssertNil(dict[4], "The dictionary should no longer contain key 4.")
    }

    // Test: No key-value pair satisfies the condition
    func testRemoveWhere_NoElementRemoved() {
        var dict: [Int: String] = [1: "One", 2: "Two", 3: "Three", 4: "Four", 5: "Five"]
        
        // Try to remove a key-value pair where the value's length is greater than 10
        let removed = dict.remove(where: { $0.value.count > 10 })
        
        XCTAssertNil(removed, "No element should be removed when no match is found.")
    }

    // Test: Removing a key-value pair from an empty dictionary
    func testRemoveWhere_EmptyDictionary() {
        var dict: [Int: String] = [:]
        
        // Try to remove a key-value pair from an empty dictionary
        let removed = dict.remove(where: { $0.key > 3 })
        
        XCTAssertNil(removed, "No element should be removed from an empty dictionary.")
    }

    // Test: Multiple key-value pairs satisfy the condition
    func testRemoveWhere_MultipleElements() {
        var dict: [Int: String] = [1: "One", 2: "Two", 3: "Three", 4: "Four", 5: "Five"]
        
        // Remove the first key-value pair where the key is greater than 2
        if let removed = dict.remove(where: { $0.key > 2 }) {
            XCTAssertEqual(removed.key, 3, "The removed key should be 3.")
            XCTAssertEqual(removed.value, "Three", "The removed value should be 'Three'.")
        } else {
            XCTFail("Expected to remove an element, but none was removed.")
        }
    }
}

// Call to the test suite (not needed in your final PR, this is for local testing)
DictionaryTests.defaultTestSuite.run()
