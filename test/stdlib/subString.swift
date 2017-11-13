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
	expectTrue("" == s.removingPrefix(s.count))
	expectTrue(s.removingFirst().removingPrefix(s.count) == s.removingPrefix(s.count))
  
  expectEqual("ab" as String, s.prefix(2))
  expectEqual("fg" as String, s.suffix(2))
  
#if _runtime(_ObjC)
  let emoji: String = s + "😄👍🏽🇫🇷👩‍👩‍👧‍👦🙈" + "😡🇧🇪🇨🇦🇮🇳"
  expectTrue(s == s[...])
  expectTrue(s[...] == s)
  expectTrue(s.removingPrefix(2) != s)
  expectTrue(s == s.removingPrefix(0))
  expectTrue(s != s.removingPrefix(1))
  expectTrue(s != s.removingSuffix(1))
  expectEqual(s[...], s[...])
  expectEqual(s.removingPrefix(0), s.removingPrefix(0))
  expectTrue(s == s.removingPrefix(0))
  expectTrue(s.removingPrefix(2) != s.removingPrefix(1))
  expectNotEqual(s.removingSuffix(2), s.removingSuffix(1))
  expectEqual(s.removingPrefix(1), s.removingPrefix(1))
  expectTrue(s != s[...].removingPrefix(1))
  let i = emoji.firstIndex(of: "😄")!
  expectEqual("😄👍🏽" as String, emoji[i...].prefix(2))
  expectTrue("😄👍🏽🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String == emoji[i...].removingSuffix(2))
  expectTrue("🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String == emoji[i...].removingSuffix(2).removingPrefix(2))
  expectTrue(s as String != emoji[i...].removingSuffix(2).removingPrefix(2))
  expectEqualSequence("😄👍🏽🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String, emoji[i...].removingSuffix(2))
  expectEqualSequence("🇫🇷👩‍👩‍👧‍👦🙈😡🇧🇪" as String, emoji[i...].removingSuffix(2).removingPrefix(2))
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

	expectTrue(s < s.removingFirst())
	expectFalse(s > s.removingFirst())
	expectFalse(s < s.removingLast())
	expectTrue(s > s.removingLast())
	expectTrue(s.removingFirst() < s.removingPrefix(2))
	expectFalse(s.removingFirst() > s.removingPrefix(2))
	expectFalse(s.removingLast() < s.removingSuffix(2))
	expectTrue(s.removingLast() > s.removingSuffix(2))
	expectFalse(s.removingFirst() < s.removingFirst().removingLast())
	expectTrue(s.removingFirst() > s.removingFirst().removingLast())
	expectTrue(s.removingFirst() > s)
	expectTrue(s.removingFirst() > s[...])
	expectTrue(s >= s[...])
	expectTrue(s.removingFirst() >= s.removingFirst())

	// comparable conformance
	expectEqualSequence("pen,pineapple,apple,pen".split(separator: ",").sorted(),
		["apple", "pen", "pen", "pineapple"])
}

SubstringTests.test("Filter") {
  var name = "😂Edward Woodward".removingFirst()
  var filtered = name.filter { $0 != "d" }
  expectType(Substring.self, &name)
  expectType(String.self, &filtered)
  expectEqual("Ewar Woowar", filtered)
}

SubstringTests.test("CharacterView") {
  let s = "abcdefg"
  var t = s.characters.removingPrefix(2)
  var u = t.removingPrefix(2)
  
  checkMatch(s.characters, t, t.startIndex)
  checkMatch(s.characters, t, t.index(after: t.startIndex))
  checkMatch(s.characters, t, t.index(before: t.endIndex))
  
  checkMatch(s.characters, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.index(after: u.startIndex))
  checkMatch(t, u, u.index(before: u.endIndex))
  
  expectEqual("", String(t.removingPrefix(10)))
  expectEqual("", String(t.removingSuffix(10)))
  expectEqual("", String(u.removingPrefix(10)))
  expectEqual("", String(u.removingSuffix(10)))
  
  t.replaceSubrange(t.startIndex...t.startIndex, with: ["C"])
  u.replaceSubrange(u.startIndex...u.startIndex, with: ["E"])
  expectEqual(String(u), "Efg")
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
}

SubstringTests.test("UnicodeScalars") {
  let s = "abcdefg"
  var t = s.unicodeScalars.removingPrefix(2)
  var u = t.removingPrefix(2)
  
  checkMatch(s.unicodeScalars, t, t.startIndex)
  checkMatch(s.unicodeScalars, t, t.index(after: t.startIndex))
  checkMatch(s.unicodeScalars, t, t.index(before: t.endIndex))
  
  checkMatch(s.unicodeScalars, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.index(after: u.startIndex))
  checkMatch(t, u, u.index(before: u.endIndex))
  
  expectEqual("", String(t.removingPrefix(10)))
  expectEqual("", String(t.removingSuffix(10)))
  expectEqual("", String(u.removingPrefix(10)))
  expectEqual("", String(u.removingSuffix(10)))
  
  t.replaceSubrange(t.startIndex...t.startIndex, with: ["C"])
  u.replaceSubrange(u.startIndex...u.startIndex, with: ["E"])
  expectEqual(String(u), "Efg")
  expectEqual(String(t), "Cdefg")
  expectEqual(s, "abcdefg")
}

SubstringTests.test("UTF16View") {
  let s = "abcdefg"
  let t = s.utf16.removingPrefix(2)
  let u = t.removingPrefix(2)
  
  checkMatch(s.utf16, t, t.startIndex)
  checkMatch(s.utf16, t, t.index(after: t.startIndex))
  checkMatch(s.utf16, t, t.index(before: t.endIndex))
  
  checkMatch(s.utf16, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.index(after: u.startIndex))
  checkMatch(t, u, u.index(before: u.endIndex))
  
  expectEqual("", String(t.removingPrefix(10))!)
  expectEqual("", String(t.removingSuffix(10))!)
  expectEqual("", String(u.removingPrefix(10))!)
  expectEqual("", String(u.removingSuffix(10))!)
}

SubstringTests.test("UTF8View") {
  let s = "abcdefg"
  let t = s.utf8.removingPrefix(2)
  let u = t.removingPrefix(2)
  
  checkMatch(s.utf8, t, t.startIndex)
  checkMatch(s.utf8, t, t.index(after: t.startIndex))
  
  checkMatch(s.utf8, t, u.startIndex)
  checkMatch(t, u, u.startIndex)
  checkMatch(t, u, u.index(after: u.startIndex))

  expectEqual("", String(t.removingPrefix(10))!)
  expectEqual("", String(t.removingSuffix(10))!)
  expectEqual("", String(u.removingPrefix(10))!)
  expectEqual("", String(u.removingSuffix(10))!)
}

SubstringTests.test("Persistent Content") {
  var str = "abc"
  str += "def"
  expectEqual("bcdefg", str.removingPrefix(1) + "g")
  expectEqual("bcdefg", (str.removingPrefix(1) + "g") as String)
}

runAllTests()
