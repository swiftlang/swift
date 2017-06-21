// RUN: %target-clang -fobjc-arc %S/Inputs/NSSlowString/NSSlowString.m -c -o %t/NSSlowString.o
// RUN: %target-build-swift -I %S/Inputs/NSSlowString/ %t/NSSlowString.o %s -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import NSSlowString
import Swift

import StdlibUnittest

let tests = TestSuite("NonContiguousStrings")

// Perform expected test checks
func checkSingleForm<S: StringProtocol>(
	_ s: S, expectedCount: Int, expectedCodeUnitCount: Int?
) {
	expectEqual(expectedCount, Int(s.count))
	if let cuCount = expectedCodeUnitCount {
		expectEqual(expectedCodeUnitCount, Int(s.utf16.count))
	}

	// Now check various reversed properties
	let reversedCharacters = Array<Character>(s.reversed())

	expectEqual(Int(s.count), reversedCharacters.count)
	expectEqualSequence(s.reversed(), reversedCharacters)
	expectEqual(String(s), String(reversedCharacters.reversed()))
}
func check(
	_ s: String, expectedCount count: Int, expectedCodeUnitCount cuCount: Int
) {
	checkSingleForm(s, expectedCount: count, expectedCodeUnitCount: cuCount)

	// TODO: substring tests
	// checkSingleForm(s[...], expectedCount: count, expectedCodeUnitCount: cuCount)
	// checkSingleForm(s.dropFirst(), expectedCount: count-1, expectedCodeUnitCount: nil)
	// checkSingleForm(s.dropLast(), expectedCount: count-1, expectedCodeUnitCount: nil)
	// checkSingleForm(s.dropLast().dropFirst(), expectedCount: count-2, expectedCodeUnitCount: nil)
}

tests.test("Unicode 9 grapheme breaking") {

	// Test string lengths that correspond to smaller than our fixed size code
	// unit buffer, larger than it, and exactly it.
	let strSmall = NSSlowString(string: "a👍👩‍👩‍👧‍👦")
	let strBig = NSSlowString(string: "abcdefg👍👩‍👩‍👧‍👦")
	let strJustRight = NSSlowString(string: "abc👍👩‍👩‍👧‍👦")
	check(strSmall as String, expectedCount: 3, expectedCodeUnitCount: 14)
	check(strBig as String, expectedCount: 9, expectedCodeUnitCount: 20)
	check(strJustRight as String, expectedCount: 5, expectedCodeUnitCount: 16)
}

tests.test("Zalgo") {
	// Check that we handle absurdly long graphemes
	var zalgo = "a👩‍👩‍👧‍👦c"
	for combo in 0x300...0x36f {
		zalgo.append(String(UnicodeScalar(combo)!))
	}
	check(
		NSSlowString(string: zalgo) as String, 
		expectedCount: 3, 
		expectedCodeUnitCount: 125
	)

	// Check for interspersed zalgo and emoji
	var megaZalgo = zalgo + zalgo + zalgo + zalgo
	check(
		NSSlowString(string: megaZalgo) as String,
		expectedCount: megaZalgo.count,
		expectedCodeUnitCount: megaZalgo.utf16.count
	)
}

runAllTests()

