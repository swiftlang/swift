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

UncheckedStringTests.test("pointerConversion") {
  // Counts elements up to (but not including) the trailing NUL terminator
  // that every `UnsafePointer<Element>`/`UnsafeRawPointer` conversion of an
  // `UncheckedString` is required to produce.
  func length<Element: FixedWidthInteger>(_ p: UnsafePointer<Element>) -> Int {
    var n = 0
    while p[n] != 0 { n += 1 }
    return n
  }

  // `.small` storage.
  let small: UncheckedString<UInt8> = "abc"
  expectEqual(3, length(small))

  // `.immortal` storage -- long enough to avoid the small-string
  // optimization, and backed by a NUL-terminated literal.
  let immortal: UncheckedString<UInt8> =
    "this literal is long enough to not be small"
  expectEqual(immortal.count, length(immortal))

  // `.dynamic` storage -- grown past `SmallUncheckedStringStorage`'s
  // capacity, so it's backed by a real heap-allocated, NUL-terminated array.
  var dynamic: UncheckedString<UInt8> = "abc"
  dynamic.append(contentsOf: "defghijklmnop" as UncheckedString<UInt8>)
  expectEqual(16, length(dynamic))

  // The conversion is generic over `Element`, not just `UInt8`.
  let small16: UncheckedString<UInt16> = "abc"
  expectEqual(3, length(small16))
  var dynamic16: UncheckedString<UInt16> = "abc"
  dynamic16.append(contentsOf: "defghijklmnop" as UncheckedString<UInt16>)
  expectEqual(16, length(dynamic16))

  // `UnsafeRawPointer` is also a valid conversion target.
  func rawLength(_ p: UnsafeRawPointer) -> Int {
    return length(p.assumingMemoryBound(to: UInt8.self))
  }
  expectEqual(3, rawLength(small))
}

// A type that opts into `CustomUncheckedStringConvertible` itself, rather
// than getting it for free via `UncheckedStringProtocol`.
struct Point: CustomUncheckedStringConvertible {
  var x: Int
  var y: Int

  func withUncheckedStringRepresentation<R, Failure>(
    _ body: (Span<UInt8>) throws(Failure) -> R
  ) throws(Failure) -> R {
    let xDigits = UncheckedString<UInt8>(String(x).utf8)
    let yDigits = UncheckedString<UInt8>(String(y).utf8)
    let description: UncheckedString<UInt8> = "(\(xDigits), \(yDigits))"
    return try description.withCharacterData(body)
  }
}

UncheckedStringTests.test("interpolation") {
  // Interpolating an `UncheckedString` value works via
  // `UncheckedStringProtocol`'s default `CustomUncheckedStringConvertible`
  // conformance, with no transcoding (same width in, same width out).
  let name: UncheckedString<UInt8> = "world"
  let greeting: UncheckedString<UInt8> = "hello, \(name)!"
  expectTrue(greeting == "hello, world!" as UncheckedString<UInt8>)

  // Multiple segments and interpolations, including single-character
  // segments -- these used to be unconditionally locked to
  // `Character`/`Unicode.Scalar` regardless of context.
  let multi: UncheckedString<UInt8> = "a\(name)b\(name)c"
  expectTrue(multi == "aworldbworldc" as UncheckedString<UInt8>)

  // `\x{hh}` inside an interpolated literal's segment materializes as a
  // native-width constant, exactly like a non-interpolated literal.
  let withEscape: UncheckedString<UInt8> = "Ren\x{e9} says: \(name)"
  expectTrue(withEscape == "Ren\x{e9} says: world" as UncheckedString<UInt8>)

  // A custom `CustomUncheckedStringConvertible` conformer.
  let point = Point(x: 1, y: 2)
  let described: UncheckedString<UInt8> = "point: \(point)"
  expectTrue(described == "point: (1, 2)" as UncheckedString<UInt8>)

  // Widths other than UInt8 work too.
  let name16: UncheckedString<UInt16> = "world"
  let greeting16: UncheckedString<UInt16> = "hello, \(name16)!"
  expectTrue(greeting16 == "hello, world!" as UncheckedString<UInt16>)
}

UncheckedStringTests.test("operators") {
  // A concrete `+` overload resolves literal operands under context,
  // including single-character operands and ones containing `\x{hh}`.
  let ab_cd: UncheckedString<UInt8> = "ab" + "cd"
  expectTrue(ab_cd == "abcd" as UncheckedString<UInt8>)

  let ab_c: UncheckedString<UInt8> = "ab" + "c"
  expectTrue(ab_c == "abc" as UncheckedString<UInt8>)

  let ab_escape: UncheckedString<UInt8> = "ab" + "\x{41}"
  expectTrue(ab_escape == "abA" as UncheckedString<UInt8>)
}

UncheckedStringTests.test("singleCharacterLiterals") {
  // A single-character literal now conforms to the "possibly unchecked"
  // unicode-scalar/grapheme-cluster umbrellas, resolving to
  // `UncheckedString<Element>` under context via a compile-time-materialized
  // constant (verified separately in test/SILGen), not runtime transcoding.
  let direct: UncheckedString<UInt8> = "c"
  expectTrue(direct == UncheckedString<UInt8>(["c".utf8.first!]))

  let coerced = "c" as UncheckedString<UInt8>
  expectTrue(coerced == direct)

  let called = UncheckedString<UInt8>("c")
  expectTrue(called == direct)

  // Ordinary `Character`/`Unicode.Scalar`/`String` are unaffected.
  let ch: Character = "x"
  let sc: Unicode.Scalar = "y"
  let str: String = "z"
  expectEqual("x", String(ch))
  expectEqual("y", String(sc))
  expectEqual("z", str)
}

runAllTests()
