// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

var SubstringTests = TestSuite("SubstringTests")

func checkMatch<S: Collection, T: Collection
  where S.Index == T.Index, S.Iterator.Element == T.Iterator.Element,
  S.Iterator.Element: Equatable>(
    x: S, _ y: T, _ i: S.Index) {
  
  expectEqual(x[i], y[i])
}

let s = "abcdefg"

SubstringTests.test("CharacterView") {
  var t = s.characters.dropFirst(2)
  var u = t.dropFirst(2)
  
  checkMatch(s.characters, t, t.startIndex)
  checkMatch(s.characters, t, t.successor(of: t.startIndex))
  checkMatch(s.characters, t, t.predecessor(of: t.endIndex))
  
  t.replaceSubrange(t.startIndex...t.startIndex, with: ["C"])
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
  
  checkMatch(s.characters, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.successor(of: u.startIndex))
  checkMatch(t, u, u.predecessor(of: u.endIndex))
  
  u.replaceSubrange(u.startIndex...u.startIndex, with: ["E"])
  expectEqual(String(u), "Efg")
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
}

SubstringTests.test("UnicodeScalars") {
  var t = s.unicodeScalars.dropFirst(2)
  var u = t.dropFirst(2)
  
  checkMatch(s.unicodeScalars, t, t.startIndex)
  checkMatch(s.unicodeScalars, t, t.successor(of: t.startIndex))
  checkMatch(s.unicodeScalars, t, t.predecessor(of: t.endIndex))
  
  t.replaceSubrange(t.startIndex...t.startIndex, with: ["C"])
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
  
  checkMatch(s.unicodeScalars, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.successor(of: u.startIndex))
  checkMatch(t, u, u.predecessor(of: u.endIndex))
  
  u.replaceSubrange(u.startIndex...u.startIndex, with: ["E"])
  expectEqual(String(u), "Efg")
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
}

SubstringTests.test("UTF16View") {
  var t = s.utf16.dropFirst(2)
  var u = t.dropFirst(2)
  
  checkMatch(s.utf16, t, t.startIndex)
  checkMatch(s.utf16, t, t.successor(of: t.startIndex))
  checkMatch(s.utf16, t, t.predecessor(of: t.endIndex))
  
  checkMatch(s.utf16, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.successor(of: u.startIndex))
  checkMatch(t, u, u.predecessor(of: u.endIndex))
}

SubstringTests.test("UTF8View") {
  var t = s.utf8.dropFirst(2)
  var u = t.dropFirst(2)
  
  checkMatch(s.utf8, t, t.startIndex)
  checkMatch(s.utf8, t, t.successor(of: t.startIndex))
  
  checkMatch(s.utf8, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.successor(of: u.startIndex))
}

runAllTests()
