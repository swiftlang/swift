// RUN: mkdir -p %t
// RUN: %target-clang -fobjc-arc %S/Inputs/NSSlowString/NSSlowString.m -c -o %t/NSSlowString.o
// RUN: %target-build-swift -I %S/Inputs/NSSlowString/ %t/NSSlowString.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
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
		expectEqual(cuCount, Int(s.utf16.count))
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

	// Substring tests
	checkSingleForm(s[...], expectedCount: count, expectedCodeUnitCount: cuCount)
	checkSingleForm(s.dropFirst(), expectedCount: count-1, expectedCodeUnitCount: nil)
	checkSingleForm(s.dropLast(), expectedCount: count-1, expectedCodeUnitCount: nil)
	checkSingleForm(s.dropLast().dropFirst(), expectedCount: count-2, expectedCodeUnitCount: nil)
}

tests.test("Iterator") {
  let native = "abc👍👩‍👩‍👧‍👦de\u{0301}f"
  let opaque = NSSlowString(string: "abc👍👩‍👩‍👧‍👦de\u{0301}f") as String
  expectEqualSequence(opaque, native)
  expectEqualSequence(opaque.unicodeScalars, native.unicodeScalars)
  expectEqualSequence(opaque.utf16, native.utf16)
  expectEqualSequence(opaque.utf8, native.utf8)

  expectEqualSequence(opaque.reversed(), native.reversed())
  expectEqualSequence(opaque.unicodeScalars.reversed(), native.unicodeScalars.reversed())
  expectEqualSequence(opaque.utf16.reversed(), native.utf16.reversed())
  expectEqualSequence(opaque.utf8.reversed(), native.utf8.reversed())
}

tests.test("String-to-integer parsing") {
  let native = "1234"
  let opaque = NSSlowString(string: "1234") as String

  expectEqual(Int(opaque, radix: 16)!, Int(native, radix: 16)!)
  expectEqual(Int(opaque, radix: 15)!, Int(native, radix: 15)!)
  expectEqual(Int(opaque, radix: 10)!, Int(native, radix: 10)!)
  expectEqual(Int(opaque, radix:  8)!, Int(native, radix:  8)!)
  expectEqual(Int(opaque, radix:  5)!, Int(native, radix:  5)!)

  expectEqual(UInt16(opaque, radix: 16)!, UInt16(native, radix: 16)!)
  expectEqual(UInt16(opaque, radix: 15)!, UInt16(native, radix: 15)!)
  expectEqual(UInt16(opaque, radix: 10)!, UInt16(native, radix: 10)!)
  expectEqual(UInt16(opaque, radix:  8)!, UInt16(native, radix:  8)!)
  expectEqual(UInt16(opaque, radix:  5)!, UInt16(native, radix:  5)!)
}

tests.test("Unicode 9 grapheme breaking")
    .xfail(.osxMinor(10, 9, reason: "Mac OS X 10.9 has an old version of ICU"))
    .xfail(.iOSMajor(7, reason: "iOS 7 has an old version of ICU"))
    .code {

	// Test string lengths that correspond to smaller than our fixed size code
	// unit buffer, larger than it, and exactly it.
	let strSmall = NSSlowString(string: "a👍👩‍👩‍👧‍👦")
	let strBig = NSSlowString(string: "abcdefg👍👩‍👩‍👧‍👦")
	let strJustRight = NSSlowString(string: "abc👍👩‍👩‍👧‍👦")
	check(strSmall as String, expectedCount: 3, expectedCodeUnitCount: 14)
	check(strBig as String, expectedCount: 9, expectedCodeUnitCount: 20)
	check(strJustRight as String, expectedCount: 5, expectedCodeUnitCount: 16)
}

tests.test("Zalgo")
    .xfail(.osxMinor(10, 9, reason: "Mac OS X 10.9 has an old version of ICU"))
    .xfail(.iOSMajor(7, reason: "iOS 7 has an old version of ICU"))
    .code {

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

