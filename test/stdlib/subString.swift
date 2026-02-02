// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// UNSUPPORTED: freestanding

import StdlibUnittest

var SubstringTests = TestSuite("SubstringTests")

func checkMatch<S: Collection, T: Collection>(_ x: S, _ y: T, _ i: S.Index)
  where S.Index == T.Index, S.Iterator.Element == T.Iterator.Element,
  S.Iterator.Element: Equatable
{
  expectEqual(x[i], y[i])
}

func checkMatchContiguousStorage<S: Collection, T: Collection>(_ x: S, _ y: T)
  where S.Element == T.Element, S.Element: Equatable
{
  let xElement = x.withContiguousStorageIfAvailable { $0.first }
  let yElement = y.withContiguousStorageIfAvailable { $0.first }

  expectEqual(xElement, yElement)
}

func checkHasContiguousStorage<S: Collection>(_ x: S) {
  expectTrue(x.withContiguousStorageIfAvailable { _ in true } ?? false)
}

func checkHasContiguousStorageSubstring(_ x: Substring.UTF8View) {
  let hasStorage = x.withContiguousStorageIfAvailable { _ in true } ?? false
  expectTrue(hasStorage)
}

fileprivate func slices(
  _ s: String,
  from: Int,
  to: Int
) -> (
  Substring,
  Substring,
  Substring
) {
  let s1 = s[s.index(s.startIndex, offsetBy: from) ..<
    s.index(s.startIndex, offsetBy: to)]
  let s2 = s1[s1.startIndex..<s1.endIndex]
  let s3 = s2[s1.startIndex..<s1.endIndex]
  return (s1, s2, s3)
}

fileprivate func allNotEmpty(
  _ s: Substring...
) -> Bool {
  s.allSatisfy { $0.isEmpty == false }
}

fileprivate func allEqual(
  _ s: Substring...
) -> Bool {
  for i in 0..<s.count {
    for j in (i + 1)..<s.count {
      if s[i] != s[j] {
        return false
      }
    }
  }
  return true
}

SubstringTests.test("Equality") {
  let s = "abcdefg"
  let s1 = s[s.index(s.startIndex, offsetBy: 2) ..<
    s.index(s.startIndex, offsetBy: 4)]
  let s2 = s1[s1.startIndex..<s1.endIndex]
  let s3 = s2[s1.startIndex..<s1.endIndex]
  
  expectEqual(s1, "cd")
  expectEqual(s2, "cd")
  expectEqual(s3, "cd")
	expectTrue("" == s.dropFirst(s.count))
	expectTrue(s.dropFirst().dropFirst(s.count) == s.dropFirst(s.count))
  
  expectEqual("ab" as String, s.prefix(2))
  expectEqual("fg" as String, s.suffix(2))
  
#if _runtime(_ObjC)
  expectTrue(s == s[...])
  expectTrue(s[...] == s)
  expectTrue(s.dropFirst(2) != s)
  expectTrue(s == s.dropFirst(0))
  expectTrue(s != s.dropFirst(1))
  expectTrue(s != s.dropLast(1))
  expectEqual(s[...], s[...])
  expectEqual(s.dropFirst(0), s.dropFirst(0))
  expectTrue(s == s.dropFirst(0))
  expectTrue(s.dropFirst(2) != s.dropFirst(1))
  expectNotEqual(s.dropLast(2), s.dropLast(1))
  expectEqual(s.dropFirst(1), s.dropFirst(1))
  expectTrue(s != s[...].dropFirst(1))
#endif

	// equatable conformance
	expectTrue("one,two,three".split(separator: ",").contains("two"))
	expectTrue("one,two,three".split(separator: ",") == ["one","two","three"])
}

#if _runtime(_ObjC)
SubstringTests.test("Equality/Emoji")
    .xfail(.osxMinor(10, 9, reason: "Mac OS X 10.9 has an old ICU"))
    .xfail(.iOSMajor(7, reason: "iOS 7 has an old ICU"))
    .code {
  let s = "abcdefg"
  let emoji: String = s + "😄👍🏽🇫🇷👩‍👩‍👧‍👦🙈" + "😡🇧🇪🇨🇦🇮🇳"
  let i = emoji.firstIndex(of: "😄")!
  expectEqual("😄👍🏽" as String, emoji[i...].prefix(2))
  expectTrue("😄👍🏽🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String == emoji[i...].dropLast(2))
  expectTrue("🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String == emoji[i...].dropLast(2).dropFirst(2))
  expectTrue(s as String != emoji[i...].dropLast(2).dropFirst(2))
  expectEqualSequence("😄👍🏽🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String, emoji[i...].dropLast(2))
  expectEqualSequence("🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String, emoji[i...].dropLast(2).dropFirst(2))
}
#endif

SubstringTests.test("Comparison") {
  var s = "abc"
	s += "defg"
	expectFalse(s < s[...])
	expectTrue(s <= s[...])
	expectTrue(s >= s[...])
	expectFalse(s > s[...])
	expectFalse(s[...] < s)
	expectTrue(s[...] <= s)
	expectTrue(s[...] >= s)
	expectFalse(s[...] > s)
	expectFalse(s[...] < s[...])
	expectTrue(s[...] <= s[...])
	expectTrue(s[...] >= s[...])
	expectFalse(s[...] > s[...])

	expectTrue(s < s.dropFirst())
	expectFalse(s > s.dropFirst())
	expectFalse(s < s.dropLast())
	expectTrue(s > s.dropLast())
	expectTrue(s.dropFirst() < s.dropFirst(2))
	expectFalse(s.dropFirst() > s.dropFirst(2))
	expectFalse(s.dropLast() < s.dropLast(2))
	expectTrue(s.dropLast() > s.dropLast(2))
	expectFalse(s.dropFirst() < s.dropFirst().dropLast())
	expectTrue(s.dropFirst() > s.dropFirst().dropLast())
	expectTrue(s.dropFirst() > s)
	expectTrue(s.dropFirst() > s[...])
	expectTrue(s >= s[...])
	expectTrue(s.dropFirst() >= s.dropFirst())

	// comparable conformance
	expectEqualSequence("pen,pineapple,apple,pen".split(separator: ",").sorted(),
		["apple", "pen", "pen", "pineapple"])
}

SubstringTests.test("Filter") {
  var name = "😂Edward Woodward".dropFirst()
  var filtered = name.filter { $0 != "d" }
  expectType(Substring.self, &name)
  expectType(String.self, &filtered)
  expectEqual("Ewar Woowar", filtered)
}

SubstringTests.test("CharacterView") {
  let s = "abcdefg"
  var t = s.dropFirst(2)
  var u = t.dropFirst(2)
  
  checkMatch(s, t, t.startIndex)
  checkMatch(s, t, t.index(after: t.startIndex))
  checkMatch(s, t, t.index(before: t.endIndex))
  
  checkMatch(s, t, u.startIndex)
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
  
  expectEqual("", String(t.dropFirst(10))!)
  expectEqual("", String(t.dropLast(10))!)
  expectEqual("", String(u.dropFirst(10))!)
  expectEqual("", String(u.dropLast(10))!)
}

SubstringTests.test("Mutate Substring through utf16 view") {
  let s = "abcdefg"
  var ss = s[...]
  expectEqual(s.startIndex, ss.startIndex)
  expectEqual(s.count, ss.count)
  let first = ss.utf16.removeFirst()
  expectEqual(first, UInt16(97))
  expectEqual(s.index(after: s.startIndex), ss.startIndex)
  expectEqual(s.count - 1, ss.count)
}

SubstringTests.test("Mutate Substring through unicodeScalars view") {
  let s = "abcdefg"
  var ss = s[...]
  expectEqual(s.startIndex, ss.startIndex)
  expectEqual(s.count, ss.count)
  ss.unicodeScalars.append("h")
  expectEqual(s.startIndex, ss.startIndex)
  expectEqual(s.count + 1, ss.count)
  expectEqual(ss.last, "h")
  expectEqual(s.last, "g")
}

SubstringTests.test("UTF8View") {
  let strs = [
    "abcdefg", // Small ASCII
    "abéÏ", // Small Unicode
    "012345678901234567890", // Large ASCII
    "abéÏ012345678901234567890", // Large Unicode
  ]

  for s in strs {
    let t = s.utf8.dropFirst(2)
    let u = t.dropFirst(2)

    checkMatch(s.utf8, t, t.startIndex)
    checkMatch(s.utf8, t, t.index(after: t.startIndex))

    checkMatch(s.utf8, t, u.startIndex)
    checkMatch(t, u, u.startIndex)
    checkMatch(t, u, u.index(after: u.startIndex))

    expectEqual("", String(t.dropFirst(100))!)
    expectEqual("", String(t.dropLast(100))!)
    expectEqual("", String(u.dropFirst(100))!)
    expectEqual("", String(u.dropLast(100))!)


    checkHasContiguousStorage(s.utf8) // Strings always do
    checkHasContiguousStorageSubstring(t)
    checkHasContiguousStorageSubstring(u)
    checkMatchContiguousStorage(Array(s.utf8), s.utf8)

    // The specialization for Substring.UTF8View.withContiguousStorageIfAvailable
    // was added in https://github.com/apple/swift/pull/29146.
    guard #available(SwiftStdlib 5.3, *) else {
      return
    }
    checkHasContiguousStorage(t)
    checkHasContiguousStorage(u)
    checkMatchContiguousStorage(Array(t), t)
    checkMatchContiguousStorage(Array(u), u)
  }
}

SubstringTests.test("Persistent Content") {
  var str = "abc"
  str += "def"
  expectEqual("bcdefg", str.dropFirst(1) + "g")
  expectEqual("bcdefg", (str.dropFirst(1) + "g") as String)
}

SubstringTests.test("Substring.base") {
  let str = "abéÏ01😓🎃👨‍👨‍👧‍👦"
  expectEqual(str, str.dropLast().base)
  for idx in str.indices {
    expectEqual(str, str[idx...].base)
    expectEqual(str, str[...idx].base)
  }
}

SubstringTests.test("isTriviallyIdentical(to:) small ascii")
.skip(.custom(
  { if #available(StdlibDeploymentTarget 6.4, *) { false } else { true } },
  reason: "Requires Swift 6.4's standard library"
))
.code {
  guard #available(StdlibDeploymentTarget 6.4, *) else { return }

  let a = "Hello"
  let b = "Hello"

  precondition(a == b)

  let (a1, a2, a3) = slices(a, from: 2, to: 4)
  let (b1, b2, b3) = slices(b, from: 2, to: 4)

  precondition(allNotEmpty(a1, a2, a3, b1, b2, b3))
  precondition(allEqual(a1, a2, a3, b1, b2, b3))

  expectTrue(a1.isTriviallyIdentical(to: a1))
  expectTrue(a1.isTriviallyIdentical(to: a2))
  expectTrue(a1.isTriviallyIdentical(to: a3))
  expectTrue(a1.isTriviallyIdentical(to: b1))
  expectTrue(a1.isTriviallyIdentical(to: b2))
  expectTrue(a1.isTriviallyIdentical(to: b3))

  expectTrue(a2.isTriviallyIdentical(to: a1))
  expectTrue(a2.isTriviallyIdentical(to: a2))
  expectTrue(a2.isTriviallyIdentical(to: a3))
  expectTrue(a2.isTriviallyIdentical(to: b1))
  expectTrue(a2.isTriviallyIdentical(to: b2))
  expectTrue(a2.isTriviallyIdentical(to: b3))

  expectTrue(a3.isTriviallyIdentical(to: a1))
  expectTrue(a3.isTriviallyIdentical(to: a2))
  expectTrue(a3.isTriviallyIdentical(to: a3))
  expectTrue(a3.isTriviallyIdentical(to: b1))
  expectTrue(a3.isTriviallyIdentical(to: b2))
  expectTrue(a3.isTriviallyIdentical(to: b3))

  let c = "Hello"

  precondition(b == c)

  let (c1, c2, c3) = slices(c, from: 1, to: 3)

  expectFalse(a1.isTriviallyIdentical(to: c1))
  expectFalse(a1.isTriviallyIdentical(to: c2))
  expectFalse(a1.isTriviallyIdentical(to: c3))

  expectFalse(a2.isTriviallyIdentical(to: c1))
  expectFalse(a2.isTriviallyIdentical(to: c2))
  expectFalse(a2.isTriviallyIdentical(to: c3))

  expectFalse(a3.isTriviallyIdentical(to: c1))
  expectFalse(a3.isTriviallyIdentical(to: c2))
  expectFalse(a3.isTriviallyIdentical(to: c3))
}

SubstringTests.test("isTriviallyIdentical(to:) small unicode")
.skip(.custom(
  { if #available(StdlibDeploymentTarget 6.4, *) { false } else { true } },
  reason: "Requires Swift 6.4's standard library"
))
.code {
  guard #available(StdlibDeploymentTarget 6.4, *) else { return }

  let a = "Cafe\u{301}"
  let b = "Cafe\u{301}"
  let c = "Café"

  precondition(a == b)
  precondition(b == c)

  let (a1, a2, a3) = slices(a, from: 2, to: 4)
  let (b1, b2, b3) = slices(b, from: 2, to: 4)
  let (c1, c2, c3) = slices(c, from: 2, to: 4)

  precondition(allNotEmpty(a1, a2, a3, b1, b2, b3, c1, c2, c3))
  precondition(allEqual(a1, a2, a3, b1, b2, b3, c1, c2, c3))

  expectTrue(a1.isTriviallyIdentical(to: a1))
  expectTrue(a1.isTriviallyIdentical(to: a2))
  expectTrue(a1.isTriviallyIdentical(to: a3))
  expectTrue(a1.isTriviallyIdentical(to: b1))
  expectTrue(a1.isTriviallyIdentical(to: b2))
  expectTrue(a1.isTriviallyIdentical(to: b3))
  expectFalse(a1.isTriviallyIdentical(to: c1))
  expectFalse(a1.isTriviallyIdentical(to: c2))
  expectFalse(a1.isTriviallyIdentical(to: c3))

  expectTrue(a2.isTriviallyIdentical(to: a1))
  expectTrue(a2.isTriviallyIdentical(to: a2))
  expectTrue(a2.isTriviallyIdentical(to: a3))
  expectTrue(a2.isTriviallyIdentical(to: b1))
  expectTrue(a2.isTriviallyIdentical(to: b2))
  expectTrue(a2.isTriviallyIdentical(to: b3))
  expectFalse(a2.isTriviallyIdentical(to: c1))
  expectFalse(a2.isTriviallyIdentical(to: c2))
  expectFalse(a2.isTriviallyIdentical(to: c3))

  expectTrue(a3.isTriviallyIdentical(to: a1))
  expectTrue(a3.isTriviallyIdentical(to: a2))
  expectTrue(a3.isTriviallyIdentical(to: a3))
  expectTrue(a3.isTriviallyIdentical(to: b1))
  expectTrue(a3.isTriviallyIdentical(to: b2))
  expectTrue(a3.isTriviallyIdentical(to: b3))
  expectFalse(a3.isTriviallyIdentical(to: c1))
  expectFalse(a3.isTriviallyIdentical(to: c2))
  expectFalse(a3.isTriviallyIdentical(to: c3))
}

SubstringTests.test("isTriviallyIdentical(to:) large ascii")
.skip(.custom(
  { if #available(StdlibDeploymentTarget 6.4, *) { false } else { true } },
  reason: "Requires Swift 6.4's standard library"
))
.code {
  guard #available(StdlibDeploymentTarget 6.4, *) else { return }

  let a = String(repeating: "foo", count: 1000)
  let b = String(repeating: "foo", count: 1000)

  precondition(a == b)

  let (a1, a2, a3) = slices(a, from: 2, to: 4)
  let (b1, b2, b3) = slices(b, from: 2, to: 4)

  precondition(allNotEmpty(a1, a2, a3, b1, b2, b3))
  precondition(allEqual(a1, a2, a3, b1, b2, b3))

  expectTrue(a1.isTriviallyIdentical(to: a1))
  expectTrue(a1.isTriviallyIdentical(to: a2))
  expectTrue(a1.isTriviallyIdentical(to: a3))
  expectFalse(a1.isTriviallyIdentical(to: b1))
  expectFalse(a1.isTriviallyIdentical(to: b2))
  expectFalse(a1.isTriviallyIdentical(to: b3))

  expectTrue(a2.isTriviallyIdentical(to: a1))
  expectTrue(a2.isTriviallyIdentical(to: a2))
  expectTrue(a2.isTriviallyIdentical(to: a3))
  expectFalse(a2.isTriviallyIdentical(to: b1))
  expectFalse(a2.isTriviallyIdentical(to: b2))
  expectFalse(a2.isTriviallyIdentical(to: b3))

  expectTrue(a3.isTriviallyIdentical(to: a1))
  expectTrue(a3.isTriviallyIdentical(to: a2))
  expectTrue(a3.isTriviallyIdentical(to: a3))
  expectFalse(a3.isTriviallyIdentical(to: b1))
  expectFalse(a3.isTriviallyIdentical(to: b2))
  expectFalse(a3.isTriviallyIdentical(to: b3))
}

runAllTests()
