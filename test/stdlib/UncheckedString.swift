// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking)
// REQUIRES: executable_test
// REQUIRES: reflection

import StdlibUnittest

var UncheckedStringTests = TestSuite("UncheckedString")

UncheckedStringTests.test("hasPrefix/hasSuffix") {
  let s: UncheckedString<UInt16> = "Ren\u{e9} Descartes"

  expectTrue(s.hasPrefix("Ren\u{e9}" as UncheckedString<UInt16>))
  expectTrue(s.hasSuffix("Descartes" as UncheckedString<UInt16>))

  expectFalse(s.hasPrefix("Renee" as UncheckedString<UInt16>))
  expectFalse(s.hasSuffix("Descartez" as UncheckedString<UInt16>))

  // A string is its own prefix and suffix.
  expectTrue(s.hasPrefix(s))
  expectTrue(s.hasSuffix(s))

  // The empty string is a prefix/suffix of everything, but nothing
  // (except itself) is a prefix/suffix of the empty string.
  let empty: UncheckedString<UInt16> = ""
  expectTrue(s.hasPrefix(empty))
  expectTrue(s.hasSuffix(empty))
  expectTrue(empty.hasPrefix(empty))
  expectTrue(empty.hasSuffix(empty))
  expectFalse(empty.hasPrefix(s))
  expectFalse(empty.hasSuffix(s))

  // A prefix/suffix longer than the string itself never matches.
  let tooLong: UncheckedString<UInt16> = "Ren\u{e9} Descartes and friends"
  expectFalse(s.hasPrefix(tooLong))
  expectFalse(s.hasSuffix(tooLong))

  // `\x{hh}` raw code unit escapes participate like any other content.
  let raw: UncheckedString<UInt8> = "Ren\x{e9} Descartes"
  expectTrue(raw.hasPrefix("Ren\x{e9}" as UncheckedString<UInt8>))
  expectTrue(raw.hasSuffix("Descartes" as UncheckedString<UInt8>))
  expectFalse(raw.hasPrefix("Ren\x{e8}" as UncheckedString<UInt8>))

  // Cross-type: an `UncheckedSubString` prefix/suffix of an `UncheckedString`.
  let sub = s[s.startIndex..<s.index(s.startIndex, offsetBy: 3)]
  expectTrue(s.hasPrefix(sub))
}

UncheckedStringTests.test("isTriviallyIdentical/empty") {
  let a: UncheckedString<UInt16> = ""
  let b: UncheckedString<UInt16> = ""
  expectTrue(a.isTriviallyIdentical(to: b))
}

UncheckedStringTests.test("isTriviallyIdentical/small") {
  // Short enough to use the small-string representation.
  let a: UncheckedString<UInt16> = "abc"
  let b = a
  expectTrue(a.isTriviallyIdentical(to: b))

  // Equal content constructed independently is not required to be
  // (and, for the small-string case, generally won't be) identical.
  // NOTE: deliberately not `"ab" + "c"` here -- a string literal used as
  // an *operand* to an operator (rather than the direct initializer of a
  // typed `let`/`var`) doesn't have a contextual type available at the
  // point its own literal protocol is chosen, so it can't route through
  // `ExpressibleByUncheckedStringLiteral`; this is a known, currently
  // unsupported case, distinct from what this test is checking.
  var c: UncheckedString<UInt16> = "ab"
  c.append(contentsOf: "c" as UncheckedString<UInt16>)
  expectTrue(a == c)

  let d: UncheckedString<UInt16> = "abd"
  expectFalse(a.isTriviallyIdentical(to: d))
}

UncheckedStringTests.test("isTriviallyIdentical/immortal") {
  // A literal with no escapes at all is long enough to avoid the
  // small-string optimization and is backed by immortal storage.
  let a: UncheckedString<UInt16> = "this literal is long enough to not be small"
  let b = a
  expectTrue(a.isTriviallyIdentical(to: b))

  let c: UncheckedString<UInt16> = "this literal is long enough to not be small"
  expectTrue(a == c)
  expectFalse(a.isTriviallyIdentical(to: c))
}

UncheckedStringTests.test("isTriviallyIdentical/dynamic") {
  var a: UncheckedString<UInt16> = "abc"
  a.append(contentsOf: "def" as UncheckedString<UInt16>)
  var b = a
  expectTrue(a.isTriviallyIdentical(to: b))

  // Mutating a copy-on-write value should not affect the original, and
  // the two should no longer be identical afterwards.
  b.append(contentsOf: "ghi" as UncheckedString<UInt16>)
  expectFalse(a.isTriviallyIdentical(to: b))
  expectTrue(a == "abcdef" as UncheckedString<UInt16>)
  expectTrue(b == "abcdefghi" as UncheckedString<UInt16>)
}

UncheckedStringTests.test("isTriviallyIdentical/differentStorageKinds") {
  // Same content, but one is small and the other has been forced into
  // dynamic storage -- these must not be considered identical even
  // though they compare equal. `SmallUncheckedStringStorage<UInt16>`'s
  // capacity is 7 elements on 64-bit platforms, so appending enough
  // extra content to push well past that (then trimming back down)
  // guarantees `dynamic` actually left small storage, rather than never
  // having exceeded capacity in the first place.
  let small: UncheckedString<UInt16> = "abc"
  var dynamic: UncheckedString<UInt16> = "abc"
  dynamic.append(contentsOf: "defghijklmnop" as UncheckedString<UInt16>)
  dynamic.removeLast(13)

  expectTrue(small == dynamic)
  expectFalse(small.isTriviallyIdentical(to: dynamic))
}

UncheckedStringTests.test("isTriviallyIdentical/substring") {
  let s: UncheckedString<UInt16> = "Ren\u{e9} Descartes"
  let sub1 = s[s.startIndex..<s.index(s.startIndex, offsetBy: 3)]
  let sub2 = s[s.startIndex..<s.index(s.startIndex, offsetBy: 3)]
  expectTrue(sub1.isTriviallyIdentical(to: sub2))

  let sub3 = s[s.index(after: s.startIndex)..<s.index(s.startIndex, offsetBy: 3)]
  expectFalse(sub1.isTriviallyIdentical(to: sub3))
}

runAllTests()
