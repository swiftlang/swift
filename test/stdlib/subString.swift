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
  let emoji: String = s + "😄👍🏽🇫🇷👩‍👩‍👧‍👦🙈" + "😡🇧🇪🇨🇦🇮🇳"
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
  let i = emoji.firstIndex(of: "😄")!
  expectEqual("😄👍🏽" as String, emoji[i...].prefix(2))
  expectTrue("😄👍🏽🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String == emoji[i...].dropLast(2))
  expectTrue("🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String == emoji[i...].dropLast(2).dropFirst(2))
  expectTrue(s as String != emoji[i...].dropLast(2).dropFirst(2))
  expectEqualSequence("😄👍🏽🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String, emoji[i...].dropLast(2))
  expectEqualSequence("🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String, emoji[i...].dropLast(2).dropFirst(2))
#endif
	// equatable conformance
	expectTrue("one,two,three".split(separator: ",").contains("two"))
	expectTrue("one,two,three".split(separator: ",") == ["one","two","three"])
}

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
    let count = s.count
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
  }
}

SubstringTests.test("Persistent Content") {
  var str = "abc"
  str += "def"
  expectEqual("bcdefg", str.dropFirst(1) + "g")
  expectEqual("bcdefg", (str.dropFirst(1) + "g") as String)
}

runAllTests()
