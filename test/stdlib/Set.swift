// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import XCTest

// Assuming the Set.swift test file is using XCTest
class SetTests: XCTestCase {

    // Existing test case, as you provided
    func setAlgebraOps<S: SetAlgebra>(_ s: S) {}

    func testSetAlgebraOperations() {
        setAlgebraOps(Set<Int>())
    }

    // Your new test cases for remove(where:)

    // Test: Remove an element that satisfies the condition
    func testRemoveWhere_ElementRemoved() {
        var set: Set = [1, 2, 3, 4, 5]
        
        // Remove the first element greater than 3
        if let removed = set.remove(where: { $0 > 3 }) {
            XCTAssertEqual(removed, 4, "The removed element should be 4.")
        } else {
            XCTFail("Expected to remove an element, but none was removed.")
        }
        
        // Verify the set after removal
        XCTAssertFalse(set.contains(4), "The set should no longer contain 4.")
    }

    // Test: No element satisfies the condition
    func testRemoveWhere_NoElementRemoved() {
        var set: Set = [1, 2, 3, 4, 5]
        
        // Try to remove an element that doesn't exist (e.g., greater than 10)
        let removed = set.remove(where: { $0 > 10 })
        
        XCTAssertNil(removed, "No element should be removed when no match is found.")
    }

    // Test: Removing an element from an empty set
    func testRemoveWhere_EmptySet() {
        var set: Set = []
        
        // Try to remove an element from an empty set
        let removed = set.remove(where: { $0 > 3 })
        
        XCTAssertNil(removed, "No element should be removed from an empty set.")
    }

    // Test: Multiple elements satisfy the condition
    func testRemoveWhere_MultipleElements() {
        var set: Set = [1, 2, 3, 4, 5]
        
        // Remove the first element greater than 2
        if let removed = set.remove(where: { $0 > 2 }) {
            XCTAssertEqual(removed, 3, "The removed element should be 3.")
        } else {
            XCTFail("Expected to remove an element, but none was removed.")
        }
    }
}

// Call to the test suite (not needed in your final PR, this is for local testing)
SetTests.defaultTestSuite.run()
