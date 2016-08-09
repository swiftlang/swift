// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
runAllTests()
#endif

