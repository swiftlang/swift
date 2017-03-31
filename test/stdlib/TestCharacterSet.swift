// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

#if FOUNDATION_XCTEST
import XCTest
class TestCharacterSetSuper : XCTestCase { }
#else
import StdlibUnittest
class TestCharacterSetSuper { }
#endif

class TestCharacterSet : TestCharacterSetSuper {
    let capitalA = UnicodeScalar(0x0041)! // LATIN CAPITAL LETTER A
    let capitalB = UnicodeScalar(0x0042)! // LATIN CAPITAL LETTER B
    let capitalC = UnicodeScalar(0x0043)! // LATIN CAPITAL LETTER C
    
    func testBasicConstruction() {
        // Create a character set
        let cs = CharacterSet.letters
        
        // Use some method from it
        let invertedCs = cs.inverted
        expectTrue(!invertedCs.contains(capitalA), "Character set must not contain our letter")
        
        // Use another method from it
        let originalCs = invertedCs.inverted
        
        expectTrue(originalCs.contains(capitalA), "Character set must contain our letter")
    }
    
    func testMutability_copyOnWrite() {
        var firstCharacterSet = CharacterSet(charactersIn: "ABC")
        expectTrue(firstCharacterSet.contains(capitalA), "Character set must contain our letter")
        expectTrue(firstCharacterSet.contains(capitalB), "Character set must contain our letter")
        expectTrue(firstCharacterSet.contains(capitalC), "Character set must contain our letter")
        
        // Make a 'copy' (just the struct)
        var secondCharacterSet = firstCharacterSet
        // first: ABC, second: ABC
        
        // Mutate first and verify that it has correct content
        firstCharacterSet.remove(charactersIn: "A")
        // first: BC, second: ABC
        
        expectTrue(!firstCharacterSet.contains(capitalA), "Character set must not contain our letter")
        expectTrue(secondCharacterSet.contains(capitalA), "Copy should not have been mutated")
        
        // Make a 'copy' (just the struct) of the second set, mutate it
        let thirdCharacterSet = secondCharacterSet
        // first: BC, second: ABC, third: ABC
        
        secondCharacterSet.remove(charactersIn: "B")
        // first: BC, second: AC, third: ABC
        
        expectTrue(firstCharacterSet.contains(capitalB), "Character set must contain our letter")
        expectTrue(!secondCharacterSet.contains(capitalB), "Character set must not contain our letter")
        expectTrue(thirdCharacterSet.contains(capitalB), "Character set must contain our letter")
        
        firstCharacterSet.remove(charactersIn: "C")
        // first: B, second: AC, third: ABC
        
        expectTrue(!firstCharacterSet.contains(capitalC), "Character set must not contain our letter")
        expectTrue(secondCharacterSet.contains(capitalC), "Character set must not contain our letter")
        expectTrue(thirdCharacterSet.contains(capitalC), "Character set must contain our letter")
    }

    func testMutability_mutableCopyCrash() {
        let cs = CharacterSet(charactersIn: "ABC")
        (cs as NSCharacterSet).mutableCopy() // this should not crash
    }
    
    func testMutability_SR_1782() {
        var nonAlphanumeric = CharacterSet.alphanumerics.inverted
        nonAlphanumeric.remove(charactersIn: " ") // this should not crash
    }

    func testRanges() {
        // Simple range check
        let asciiUppercase = CharacterSet(charactersIn: UnicodeScalar(0x41)!...UnicodeScalar(0x5A)!)
        expectTrue(asciiUppercase.contains(UnicodeScalar(0x49)!))
        expectTrue(asciiUppercase.contains(UnicodeScalar(0x5A)!))
        expectTrue(asciiUppercase.contains(UnicodeScalar(0x41)!))
        expectTrue(!asciiUppercase.contains(UnicodeScalar(0x5B)!))
        
        // Some string filtering tests
        let asciiLowercase = CharacterSet(charactersIn: UnicodeScalar(0x61)!...UnicodeScalar(0x7B)!)
        let testString = "helloHELLOhello"
        let expected = "HELLO"
        
        let result = testString.trimmingCharacters(in: asciiLowercase)
        expectEqual(result, expected)
    }

    func testClosedRanges_SR_2988() {
      // "CharacterSet.insert(charactersIn: ClosedRange) crashes on a closed ClosedRange<UnicodeScalar> containing U+D7FF"
      let problematicChar = UnicodeScalar(0xD7FF)!
      let range = capitalA...problematicChar
      var characters = CharacterSet(charactersIn: range) // this should not crash
      expectTrue(characters.contains(problematicChar))
      characters.remove(charactersIn: range) // this should not crash
      expectTrue(!characters.contains(problematicChar))
      characters.insert(charactersIn: range) // this should not crash
      expectTrue(characters.contains(problematicChar))
    }

    func testInsertAndRemove() {
        var asciiUppercase = CharacterSet(charactersIn: UnicodeScalar(0x41)!...UnicodeScalar(0x5A)!)
        expectTrue(asciiUppercase.contains(UnicodeScalar(0x49)!))
        expectTrue(asciiUppercase.contains(UnicodeScalar(0x5A)!))
        expectTrue(asciiUppercase.contains(UnicodeScalar(0x41)!))
        
        asciiUppercase.remove(UnicodeScalar(0x49)!)
        expectTrue(!asciiUppercase.contains(UnicodeScalar(0x49)!))
        expectTrue(asciiUppercase.contains(UnicodeScalar(0x5A)!))
        expectTrue(asciiUppercase.contains(UnicodeScalar(0x41)!))
       

        // Zero-length range
        asciiUppercase.remove(charactersIn: UnicodeScalar(0x41)!..<UnicodeScalar(0x41)!)
        expectTrue(asciiUppercase.contains(UnicodeScalar(0x41)!))

        asciiUppercase.remove(charactersIn: UnicodeScalar(0x41)!..<UnicodeScalar(0x42)!)
        expectTrue(!asciiUppercase.contains(UnicodeScalar(0x41)!))
        
        asciiUppercase.remove(charactersIn: "Z")
        expectTrue(!asciiUppercase.contains(UnicodeScalar(0x5A)))
    }
    
    func testBasics() {
        
        var result : [String] = []
        
        let string = "The quick, brown, fox jumps over the lazy dog - because, why not?"
        var set = CharacterSet(charactersIn: ",-")
        result = string.components(separatedBy: set)
        expectEqual(5, result.count)
        
        set.remove(charactersIn: ",")
        set.insert(charactersIn: " ")
        result = string.components(separatedBy: set)
        expectEqual(14, result.count)
        
        set.remove(" ".unicodeScalars.first!)
        result = string.components(separatedBy: set)
        expectEqual(2, result.count)
    }

    // MARK: -
    func test_classForCoder() {
        // confirm internal bridged impl types are not exposed to archival machinery
        let cs = CharacterSet() as NSCharacterSet
        let expected: AnyClass = NSCharacterSet.self as AnyClass
        expectTrue(cs.classForCoder == expected)
        expectTrue(cs.classForKeyedArchiver == expected)
    }

    func test_AnyHashableContainingCharacterSet() {
        let values: [CharacterSet] = [
            CharacterSet(charactersIn: "ABC"),
            CharacterSet(charactersIn: "XYZ"),
            CharacterSet(charactersIn: "XYZ")
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(CharacterSet.self, type(of: anyHashables[0].base))
        expectEqual(CharacterSet.self, type(of: anyHashables[1].base))
        expectEqual(CharacterSet.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSCharacterSet() {
        let values: [NSCharacterSet] = [
            NSCharacterSet(charactersIn: "ABC"),
            NSCharacterSet(charactersIn: "XYZ"),
            NSCharacterSet(charactersIn: "XYZ"),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(CharacterSet.self, type(of: anyHashables[0].base))
        expectEqual(CharacterSet.self, type(of: anyHashables[1].base))
        expectEqual(CharacterSet.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_superSet() {
        let a = CharacterSet.letters.isSuperset(of: CharacterSet(charactersIn: "ab"))
        expectTrue(a)
    }

    func test_union() {
        let union = CharacterSet(charactersIn: "ab").union(CharacterSet(charactersIn: "cd"))
        let expected = CharacterSet(charactersIn: "abcd")
        expectEqual(expected, union)
    }

    func test_subtracting() {
        let difference = CharacterSet(charactersIn: "abc").subtracting(CharacterSet(charactersIn: "b"))
        let expected = CharacterSet(charactersIn: "ac")
        expectEqual(expected, difference)
    }

    func test_subtractEmptySet() {
        var mutableSet = CharacterSet(charactersIn: "abc")
        let emptySet = CharacterSet()
        mutableSet.subtract(emptySet)
        let expected = CharacterSet(charactersIn: "abc")
        expectEqual(expected, mutableSet)
    }

    func test_subtractNonEmptySet() {
        var mutableSet = CharacterSet()
        let nonEmptySet = CharacterSet(charactersIn: "abc")
        mutableSet.subtract(nonEmptySet)
        expectTrue(mutableSet.isEmpty)
    }

    func test_symmetricDifference() {
        let symmetricDifference = CharacterSet(charactersIn: "ac").symmetricDifference(CharacterSet(charactersIn: "b"))
        let expected = CharacterSet(charactersIn: "abc")
        expectEqual(expected, symmetricDifference)
    }

    func test_hasMember() {
        let contains = CharacterSet.letters.hasMember(inPlane: 1)
        expectTrue(contains)
    }

    func test_bitmap() {
        let bitmap = CharacterSet(charactersIn: "ab").bitmapRepresentation
        expectEqual(0x6, bitmap[12])
        expectEqual(8192, bitmap.count)
    }
    func test_setOperationsOfEmptySet(){
        let emptySet = CharacterSet()
        let immutableSet = CharacterSet(charactersIn:"abc")
        expectTrue(immutableSet.isSuperset(of: emptySet)) 
        expectTrue(emptySet.isSuperset(of: emptySet))
        expectFalse(emptySet.isSuperset(of: immutableSet))
        expectTrue(immutableSet.isStrictSuperset(of: emptySet))
        expectFalse(emptySet.isStrictSuperset(of: emptySet))
        expectFalse(emptySet.isStrictSuperset(of: immutableSet)) 
        expectTrue(emptySet.isSubset(of: immutableSet))
        expectTrue(emptySet.isSubset(of: emptySet))
        expectFalse(immutableSet.isSubset(of: emptySet))
        expectTrue(emptySet.isStrictSubset(of: immutableSet))
        expectFalse(emptySet.isStrictSubset(of: emptySet))
        expectFalse(immutableSet.isStrictSubset(of: emptySet))
        expectFalse(immutableSet.isStrictSubset(of: immutableSet))
        expectTrue(emptySet==emptySet)
        expectFalse(immutableSet==emptySet)
    }
    func test_moreSetOperations(){
        let firstSet = CharacterSet(charactersIn:"abc")
        let secondSet = CharacterSet(charactersIn:"abcd")
        expectTrue(firstSet==firstSet)
        expectFalse(firstSet==secondSet)
        expectTrue(firstSet.isStrictSubset(of:secondSet))
        expectFalse(secondSet.isStrictSubset(of:firstSet))
        expectTrue(secondSet.isStrictSuperset(of:firstSet))
        expectFalse(firstSet.isStrictSuperset(of:secondSet))
    }
    func test_resilienceBug(){
        // test if a fix or a workaround for rdar://29474937  is applied
        for i in 1...100{
            test_superSet()
            test_setOperationsOfEmptySet()
            test_moreSetOperations()
        }
    }
}


#if !FOUNDATION_XCTEST
var CharacterSetTests = TestSuite("TestCharacterSet")
CharacterSetTests.test("testBasicConstruction") { TestCharacterSet().testBasicConstruction() }
CharacterSetTests.test("testMutability_copyOnWrite") { TestCharacterSet().testMutability_copyOnWrite() }
CharacterSetTests.test("testMutability_mutableCopyCrash") { TestCharacterSet().testMutability_mutableCopyCrash() }
CharacterSetTests.test("testMutability_SR_1782") { TestCharacterSet().testMutability_SR_1782() }
CharacterSetTests.test("testRanges") { TestCharacterSet().testRanges() }
CharacterSetTests.test("testInsertAndRemove") { TestCharacterSet().testInsertAndRemove() }
CharacterSetTests.test("testBasics") { TestCharacterSet().testBasics() }
CharacterSetTests.test("test_classForCoder") { TestCharacterSet().test_classForCoder() }
CharacterSetTests.test("test_AnyHashableContainingCharacterSet") { TestCharacterSet().test_AnyHashableContainingCharacterSet() }
CharacterSetTests.test("test_AnyHashableCreatedFromNSCharacterSet") { TestCharacterSet().test_AnyHashableCreatedFromNSCharacterSet() }
CharacterSetTests.test("test_superSet") { TestCharacterSet().test_superSet() }
CharacterSetTests.test("test_union") { TestCharacterSet().test_union() }
CharacterSetTests.test("test_subtracting") { TestCharacterSet().test_subtracting() }
CharacterSetTests.test("test_subtractEmptySet") { TestCharacterSet().test_subtractEmptySet() }
CharacterSetTests.test("test_subtractNonEmptySet") { TestCharacterSet().test_subtractNonEmptySet() }
CharacterSetTests.test("test_symmetricDifference") { TestCharacterSet().test_symmetricDifference() }
CharacterSetTests.test("test_hasMember") { TestCharacterSet().test_hasMember() }
CharacterSetTests.test("test_bitmap") { TestCharacterSet().test_bitmap() }
CharacterSetTests.test("test_setOperationsOfEmptySet") { TestCharacterSet().test_setOperationsOfEmptySet() }
CharacterSetTests.test("test_moreSetOperations") { TestCharacterSet().test_moreSetOperations() }
CharacterSetTests.test("test_resilienceBug") { TestCharacterSet().test_resilienceBug() }

runAllTests()
#endif

