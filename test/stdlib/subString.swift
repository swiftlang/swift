// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var SubstringTests = TestSuite("SubstringTests")

func checkMatch<S: Collection, T: Collection>(_ x: S, _ y: T, _ i: S.Index)
  where S.Index == T.Index, S.Iterator.Element == T.Iterator.Element,
  S.Iterator.Element: Equatable
{
  expectEqual(x[i], y[i])
}

SubstringTests.test("String") {
  let s = "abcdefg"
  // Michael NOTE: String(Substring)
  let s1 = String(s[s.index(s.startIndex, offsetBy: 2) ..<
    s.index(s.startIndex, offsetBy: 4)])
  let s2 = String(s1[s1.startIndex..<s1.endIndex])
  let s3 = String(s2[s1.startIndex..<s1.endIndex])

  expectEqual(s1, "cd")
  expectEqual(s2, "cd")
  expectEqual(s3, "cd")
}

SubstringTests.test("CharacterView") {
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
  
  expectEqual("", String(t.dropFirst(10)))
  expectEqual("", String(t.dropLast(10)))
  expectEqual("", String(u.dropFirst(10)))
  expectEqual("", String(u.dropLast(10)))
  
  t.replaceSubrange(t.startIndex...t.startIndex, with: ["C"])
  u.replaceSubrange(u.startIndex...u.startIndex, with: ["E"])
  expectEqual(String(u), "Efg")
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
}

SubstringTests.test("UnicodeScalars") {
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
  
  expectEqual("", String(t.dropFirst(10)))
  expectEqual("", String(t.dropLast(10)))
  expectEqual("", String(u.dropFirst(10)))
  expectEqual("", String(u.dropLast(10)))
  
  t.replaceSubrange(t.startIndex...t.startIndex, with: ["C"])
  u.replaceSubrange(u.startIndex...u.startIndex, with: ["E"])
  expectEqual(String(u), "Efg")
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
}

extension String {
  // FIXME: the one(s) we put in the standard library should be smarter (work on
  // any Sequence, take advantage of underlying UTF16 codeUnits if/when they
  // exist, etc.)
  init<CodeUnits: BidirectionalCollection, Encoding: UnicodeEncoding>(
    _ c: CodeUnits, fromEncoding e: Encoding.Type
  ) where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : BidirectionalCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
    content = String.Content(utf16:
      _UnicodeViews(c, e).transcoded(to: UTF16.self))
  }
}

SubstringTests.test("UTF16View") {
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

  expectEqual("", String(t.dropFirst(10), fromEncoding: UTF16.self))
  expectEqual("", String(t.dropLast(10), fromEncoding: UTF16.self))
  expectEqual("", String(u.dropFirst(10), fromEncoding: UTF16.self))
  expectEqual("", String(u.dropLast(10), fromEncoding: UTF16.self))
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

  expectEqual("", String(t.dropFirst(10), fromEncoding: UTF8.self))
  expectEqual("", String(t.dropLast(10), fromEncoding: UTF8.self))
  expectEqual("", String(u.dropFirst(10), fromEncoding: UTF8.self))
  expectEqual("", String(u.dropLast(10), fromEncoding: UTF8.self))
}

runAllTests()
