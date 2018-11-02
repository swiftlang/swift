// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-run %t/a.out4
// REQUIRES: executable_test

// REQUIRES: objc_interop

//
// Tests for the NSString APIs on Substring
//

import StdlibUnittest

import Foundation


extension String {
  func range(fromStart: Int, fromEnd: Int) -> Range<String.Index> {
    return index(startIndex, offsetBy: fromStart) ..<
           index(endIndex, offsetBy: fromEnd)
  }
  subscript(fromStart: Int, fromEnd: Int) -> SubSequence {
    return self[range(fromStart: fromStart, fromEnd: fromEnd)]
  }
}

var tests = TestSuite("NSStringAPIs/Substring")

tests.test("range(of:)/NilRange") {
  let ss = "aabcdd"[1, -1]
  let range = ss.range(of: "bc")
  expectOptionalEqual("bc", range.map { ss[$0] })
}

tests.test("range(of:)/NonNilRange") {
  let s = "aabcdd"
  let ss = s[1, -1]
  let searchRange = s.range(fromStart: 2, fromEnd: -2)
  let range = ss.range(of: "bc", range: searchRange)
  expectOptionalEqual("bc", range.map { ss[$0] })
}

tests.test("rangeOfCharacter") {
  let ss = "__hello__"[2, -2]
  let range = ss.rangeOfCharacter(from: CharacterSet.alphanumerics)
  expectOptionalEqual("h", range.map { ss[$0] })
}

tests.test("compare(_:options:range:locale:)/NilRange") {
  let needle = "hello"
  let haystack = "__hello__"[2, -2]
  expectEqual(.orderedSame, haystack.compare(needle))
}

tests.test("compare(_:options:range:locale:)/NonNilRange") {
  let needle = "hello"
  let haystack = "__hello__"
  let range = haystack.range(fromStart: 2, fromEnd: -2)
  expectEqual(.orderedSame, haystack[range].compare(needle, range: range))
}

tests.test("replacingCharacters(in:with:)") {
  let s = "__hello, world"
  let range = s.range(fromStart: 2, fromEnd: -7)
  let expected = "__goodbye, world"
  let replacement = "goodbye"
  expectEqual(expected,
    s.replacingCharacters(in: range, with: replacement))
  expectEqual(expected[2, 0],
    s[2, 0].replacingCharacters(in: range, with: replacement))

  expectEqual(replacement,
    s.replacingCharacters(in: s.startIndex..., with: replacement))
  expectEqual(replacement,
    s.replacingCharacters(in: ..<s.endIndex, with: replacement))
  expectEqual(expected[2, 0],
    s[2, 0].replacingCharacters(in: range, with: replacement[...]))
}

tests.test("replacingOccurrences(of:with:options:range:)/NilRange") {
  let s = "hello"

  expectEqual("he11o", s.replacingOccurrences(of: "l", with: "1"))
  expectEqual("he11o", s.replacingOccurrences(of: "l"[...], with: "1"))
  expectEqual("he11o", s.replacingOccurrences(of: "l", with: "1"[...]))
  expectEqual("he11o", s.replacingOccurrences(of: "l"[...], with: "1"[...]))

  expectEqual("he11o",
    s[...].replacingOccurrences(of: "l", with: "1"))
  expectEqual("he11o",
    s[...].replacingOccurrences(of: "l"[...], with: "1"))
  expectEqual("he11o",
    s[...].replacingOccurrences(of: "l", with: "1"[...]))
  expectEqual("he11o",
    s[...].replacingOccurrences(of: "l"[...], with: "1"[...]))
}

tests.test("replacingOccurrences(of:with:options:range:)/NonNilRange") {
  let s = "hello"
  let r = s.range(fromStart: 1, fromEnd: -2)

  expectEqual("he1lo",
    s.replacingOccurrences(of: "l", with: "1", range: r))
  expectEqual("he1lo",
    s.replacingOccurrences(of: "l"[...], with: "1", range: r))
  expectEqual("he1lo",
    s.replacingOccurrences(of: "l", with: "1"[...], range: r))
  expectEqual("he1lo",
    s.replacingOccurrences(of: "l"[...], with: "1"[...], range: r))

  expectEqual("he1lo",
    s[...].replacingOccurrences(of: "l", with: "1", range: r))
  expectEqual("he1lo",
    s[...].replacingOccurrences(of: "l"[...], with: "1", range: r))
  expectEqual("he1lo",
    s[...].replacingOccurrences(of: "l", with: "1"[...], range: r))
  expectEqual("he1lo",
    s[...].replacingOccurrences(of: "l"[...], with: "1"[...], range: r))

  let ss = s[1, -1]
  expectEqual("e1l",
    ss.replacingOccurrences(of: "l", with: "1", range: r))
  expectEqual("e1l",
    ss.replacingOccurrences(of: "l"[...], with: "1", range: r))
  expectEqual("e1l",
    ss.replacingOccurrences(of: "l", with: "1"[...], range: r))
  expectEqual("e1l",
    ss.replacingOccurrences(of: "l"[...], with: "1"[...], range: r))
}

tests.test("substring(with:)") {
  let s = "hello, world"
  let r = s.range(fromStart: 7, fromEnd: 0)
  expectEqual("world", s.substring(with: r))
  expectEqual("world", s[...].substring(with: r))
  expectEqual("world", s[1, 0].substring(with: r))
}

tests.test("substring(with:)/SubscriptEquivalence") {
  let s = "hello, world"
  let r = s.range(fromStart: 7, fromEnd: 0)
  expectEqual(s[r], s.substring(with: r))
  expectEqual(s[...][r], s[...].substring(with: r))
  expectEqual(s[1, 0][r], s[1, 0].substring(with: r))
}

runAllTests()
