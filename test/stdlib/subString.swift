// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var SubstringTests = TestSuite("SubstringTests")

func checkMatch<S: Collection, T: Collection
  where S.Index == T.Index, S.Iterator.Element == T.Iterator.Element,
  S.Iterator.Element: Equatable>(
  _ x: S, _ y: T, _ i: S.Index) {
  
  expectEqual(x[i], y[i])
}

SubstringTests.test("String") {
  let s = "abcdefg"
  let s1 = s[s.index(s.startIndex, offsetBy: 2) ..<
    s.index(s.startIndex, offsetBy: 4)]
  let s2 = s1[s1.startIndex..<s1.endIndex]
  let s3 = s2[s1.startIndex..<s1.endIndex]
  
  expectEqual(s1, "cd")
  expectEqual(s2, "cd")
  expectEqual(s3, "cd")
}

SubstringTests.test("CharacterView")
  .xfail(.always("CharacterView slices don't share indices"))
  .code {
  let s = "abcdefg"
  var t = s.characters.dropFirst(2)
  var u = t.dropFirst(2)
  
  checkMatch(s.characters, t, t.startIndex)
  checkMatch(s.characters, t, t.index(after: t.startIndex))
  checkMatch(s.characters, t, t.index(before: t.endIndex))
  
  checkMatch(s.characters, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.index(after: u.startIndex))
  checkMatch(t, u, u.index(before: u.endIndex))
  
  t.replaceSubrange(t.startIndex...t.startIndex, with: ["C"])
  u.replaceSubrange(u.startIndex...u.startIndex, with: ["E"])
  expectEqual(String(u), "Efg")
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
}

SubstringTests.test("UnicodeScalars")
  .xfail(.always("UnicodeScalarsView slices don't share indices"))
  .code {
  let s = "abcdefg"
  var t = s.unicodeScalars.dropFirst(2)
  var u = t.dropFirst(2)
  
  checkMatch(s.unicodeScalars, t, t.startIndex)
  checkMatch(s.unicodeScalars, t, t.index(after: t.startIndex))
  checkMatch(s.unicodeScalars, t, t.index(before: t.endIndex))
  
  checkMatch(s.unicodeScalars, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.index(after: u.startIndex))
  checkMatch(t, u, u.index(before: u.endIndex))
  
  t.replaceSubrange(t.startIndex...t.startIndex, with: ["C"])
  u.replaceSubrange(u.startIndex...u.startIndex, with: ["E"])
  expectEqual(String(u), "Efg")
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
}

SubstringTests.test("UTF16View")
  .xfail(.always("UTF16View slices don't share indices"))
  .code {
  let s = "abcdefg"
  let t = s.utf16.dropFirst(2)
  let u = t.dropFirst(2)
  
  checkMatch(s.utf16, t, t.startIndex)
  checkMatch(s.utf16, t, t.index(after: t.startIndex))
  checkMatch(s.utf16, t, t.index(before: t.endIndex))
  
  checkMatch(s.utf16, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.index(after: u.startIndex))
  checkMatch(t, u, u.index(before: u.endIndex))
}

SubstringTests.test("UTF8View") {
  let s = "abcdefg"
  let t = s.utf8.dropFirst(2)
  let u = t.dropFirst(2)
  
  checkMatch(s.utf8, t, t.startIndex)
  checkMatch(s.utf8, t, t.index(after: t.startIndex))
  
  checkMatch(s.utf8, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.index(after: u.startIndex))
}

runAllTests()
