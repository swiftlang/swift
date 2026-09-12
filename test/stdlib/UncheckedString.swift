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
  // `append(contentsOf:)` is used here (rather than `+`, which also works --
  // see the "operators" test below) simply to build `c` via a different
  // construction path than `a`'s literal.
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
  // though they compare equal. `.dynamic` storage never demotes back to
  // `.small` on shrink (see "noStorageDemotionOnShrink" below): once a
  // string has paid for a heap allocation, further mutations are likely,
  // so the storage kind -- and the underlying buffer's reserved capacity
  // -- is kept rather than given up. `SmallUncheckedStringStorage<UInt16>`'s
  // capacity is 7 elements on 64-bit platforms, so appending enough extra
  // content to push well past that (then trimming back down) guarantees
  // `dynamic` actually left small storage, rather than never having
  // exceeded capacity in the first place.
  let small: UncheckedString<UInt16> = "abc"
  var dynamic: UncheckedString<UInt16> = "abc"
  dynamic.append(contentsOf: "defghijklmnop" as UncheckedString<UInt16>)
  dynamic.removeLast(13)

  expectTrue(small == dynamic)
  expectFalse(small.isTriviallyIdentical(to: dynamic))

  // Storage kinds that differ for reasons *other* than a size threshold --
  // e.g. immortal (a long literal, backed by static memory) vs. dynamic (the
  // same long content, but built up via mutation into a heap-allocated
  // array) -- are never candidates for demotion into each other (both are
  // well above small-storage capacity), so they should still never be
  // considered trivially identical even with equal content.
  let immortalLong: UncheckedString<UInt16> = "this literal is long enough to not be small"
  var dynamicLong: UncheckedString<UInt16> = "this literal is long eno"
  dynamicLong.append(contentsOf: "ugh to not be small" as UncheckedString<UInt16>)

  expectTrue(immortalLong == dynamicLong)
  expectFalse(immortalLong.isTriviallyIdentical(to: dynamicLong))
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

  // The three-operand chain from the proposal's own motivating example,
  // including a non-ASCII, multi-byte-in-UTF-8 character, resolves under
  // the contextual type of the `let` even though none of the individual
  // literal operands carries one on its own.
  let name: UncheckedString<UInt8> = "René" + " " + "Descartes"
  expectTrue(name == "René Descartes" as UncheckedString<UInt8>)
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

// MARK: - Performance-fix regression coverage
//
// Exhaustively exercises the allocation-free small-storage mutation paths
// (append/insert/remove/replaceSubrange) added for `Element ==
// FixedWidthInteger` in general (not just `UInt8`), checked against a plain
// `Array` oracle, and independently cross-checks the cached `.dynamic`
// `count` field (Storage.count) against the real underlying element count
// (`withCharacterData { $0.count }`) after every mutation, to catch a
// stale-cache bug that a same-source-of-truth comparison (e.g. `Array(s)`,
// whose length is itself derived from the cached count) would not.
func checkSmallStorageMutations<Element: FixedWidthInteger>(
  capacity: Int, of type: Element.Type
) {
  func checkConsistent(_ s: UncheckedString<Element>, _ oracle: [Element]) {
    expectEqual(Array(s), oracle)
    expectEqual(s.count, oracle.count)
    expectEqual(s.count, s.withCharacterData { $0.count })
  }

  for count in 0...capacity {
    let base: [Element] = (0..<count).map { Element(truncatingIfNeeded: $0 + 1) }

    if count < capacity {
      var s = UncheckedString<Element>(base)
      var oracle = base
      let e = Element(99)
      s.append(e)
      oracle.append(e)
      checkConsistent(s, oracle)

      for i in 0...count {
        var s = UncheckedString<Element>(base)
        var oracle = base
        s.insert(e, at: i)
        oracle.insert(e, at: i)
        checkConsistent(s, oracle)
      }
    }

    if count > 0 {
      for i in 0..<count {
        var s = UncheckedString<Element>(base)
        var oracle = base
        let removed = s.remove(at: i)
        let removedOracle = oracle.remove(at: i)
        expectEqual(removed, removedOracle)
        checkConsistent(s, oracle)
      }
    }

    for lo in 0...count {
      for hi in lo...count {
        for replacementLength in 0...2 {
          let finalCount = count - (hi - lo) + replacementLength
          guard finalCount <= capacity else { continue }
          var s = UncheckedString<Element>(base)
          var oracle = base
          let replacement: [Element] =
            (0..<replacementLength).map { Element(210 + $0) }
          s.replaceSubrange(lo..<hi, with: replacement)
          oracle.replaceSubrange(lo..<hi, with: replacement)
          checkConsistent(s, oracle)
        }
      }
    }
  }
}

UncheckedStringTests.test("smallStorageMutations/UInt8") {
  // `SmallUncheckedStringStorage<UInt8>`'s capacity is 14 elements on
  // 64-bit platforms (matching the convention of hardcoding this already
  // used elsewhere in this file, since the type itself isn't visible
  // outside the defining module).
  checkSmallStorageMutations(capacity: 14, of: UInt8.self)
}

UncheckedStringTests.test("smallStorageMutations/UInt16") {
  // `SmallUncheckedStringStorage<UInt16>`'s capacity is 7 elements on
  // 64-bit platforms.
  checkSmallStorageMutations(capacity: 7, of: UInt16.self)
}

// Shrinking `.dynamic` storage back down across (and below) the
// small-storage capacity boundary, whether via `replaceSubrange`-based
// APIs (`removeLast`) or `removeAll`, must *not* demote it to `.small`/
// `.empty`, for any `FixedWidthInteger` element type: a string already
// being mutated is likely to be mutated further, so the storage kind (and,
// for `removeAll(keepingCapacity: true)`, the underlying buffer) is kept
// rather than given up. Checked via `isTriviallyIdentical` against a
// freshly-constructed `.small`/`.empty` string with the same content,
// which is the sharpest check that no demotion happened (rather than
// merely comparing equal).
func checkNoStorageDemotionOnShrink<Element: FixedWidthInteger>(
  capacity: Int, of type: Element.Type
) {
  let content: [Element] = (0..<capacity).map { Element(truncatingIfNeeded: $0 + 1) }
  let small = UncheckedString<Element>(content)

  var dynamic = UncheckedString<Element>(content)
  dynamic.append(contentsOf: (0..<20).map { Element(truncatingIfNeeded: $0 + 100) })
  expectEqual(dynamic.count, capacity + 20)

  dynamic.removeLast(20)
  expectEqual(dynamic.count, capacity)
  expectTrue(small == dynamic)
  expectFalse(small.isTriviallyIdentical(to: dynamic))

  // `removeAll(keepingCapacity: true)` on a `.dynamic` source: stays
  // `.dynamic`, zero-length, not `.empty`.
  var removedKeepingCapacity = dynamic
  removedKeepingCapacity.removeAll(keepingCapacity: true)
  expectEqual(removedKeepingCapacity.count, 0)
  expectFalse(UncheckedString<Element>().isTriviallyIdentical(to: removedKeepingCapacity))

  // `removeAll(keepingCapacity: false)` on a `.dynamic` source: the caller
  // is explicitly saying it doesn't need the capacity kept, so this drops
  // straight to `.empty` -- both this type's own `removeAll` override (in
  // UncheckedString+RangeReplaceableCollection.swift) and
  // `RangeReplaceableCollection`'s default agree on this.
  var removedDiscardingCapacity = dynamic
  removedDiscardingCapacity.removeAll(keepingCapacity: false)
  expectEqual(removedDiscardingCapacity.count, 0)
  expectTrue(UncheckedString<Element>().isTriviallyIdentical(to: removedDiscardingCapacity))

  // A source that was never `.dynamic` to begin with has no buffer to
  // preserve, so `removeAll` on it still produces `.empty`.
  var neverDynamic = small
  neverDynamic.removeAll(keepingCapacity: true)
  expectEqual(neverDynamic.count, 0)
  expectTrue(UncheckedString<Element>().isTriviallyIdentical(to: neverDynamic))
}

UncheckedStringTests.test("noStorageDemotionOnShrink/UInt8") {
  checkNoStorageDemotionOnShrink(capacity: 14, of: UInt8.self)
}

UncheckedStringTests.test("noStorageDemotionOnShrink/UInt16") {
  checkNoStorageDemotionOnShrink(capacity: 7, of: UInt16.self)
}

// MARK: Codable

// A minimal, self-contained `Encoder`/`Decoder` pair backed by a flat
// `[Any]`, used only to exercise `UncheckedString`'s `Codable` conformance
// without pulling in a Foundation dependency. It supports exactly what
// that conformance needs -- a top-level unkeyed container, and single
// value containers nested underneath it for the fixed-width integer
// element types this file's tests use -- and `fatalError`s on anything
// else, since nothing else is ever exercised here.
private final class ArrayCoderStorage {
  var elements: [Any] = []
  var readIndex = 0
  // Lets a test simulate a decoder whose reported `count` doesn't match
  // the actual number of elements present.
  var countOverride: Int? = nil
}

private struct ArrayEncoder: Encoder {
  let storage: ArrayCoderStorage
  var codingPath: [any CodingKey] = []
  var userInfo: [CodingUserInfoKey: Any] = [:]

  func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key> {
    fatalError("not implemented")
  }
  func unkeyedContainer() -> any UnkeyedEncodingContainer {
    return ArrayUnkeyedEncoding(storage: storage)
  }
  func singleValueContainer() -> any SingleValueEncodingContainer {
    return ArraySingleValueEncoding(storage: storage)
  }
}

private struct ArrayUnkeyedEncoding: UnkeyedEncodingContainer {
  let storage: ArrayCoderStorage
  var codingPath: [any CodingKey] = []
  var count: Int { storage.elements.count }

  mutating func encodeNil() throws { storage.elements.append(Optional<Any>.none as Any) }
  mutating func encode(_ value: Bool) throws { fatalError("not implemented") }
  mutating func encode(_ value: String) throws { fatalError("not implemented") }
  mutating func encode(_ value: Double) throws { fatalError("not implemented") }
  mutating func encode(_ value: Float) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int8) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int16) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int32) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int64) throws { fatalError("not implemented") }
  mutating func encode(_ value: UInt) throws { fatalError("not implemented") }
  mutating func encode(_ value: UInt8) throws { fatalError("not implemented") }
  mutating func encode(_ value: UInt16) throws { fatalError("not implemented") }
  mutating func encode(_ value: UInt32) throws { fatalError("not implemented") }
  mutating func encode(_ value: UInt64) throws { fatalError("not implemented") }
  mutating func encode<T: Encodable>(_ value: T) throws {
    // `UncheckedString.encode(to:)` calls `container.encode(data[i])`
    // generically over `Element: Encodable`, so it always lands here,
    // never on one of the concrete-type overloads above -- those exist
    // only to satisfy the protocol requirement and are otherwise unused
    // by this harness. This nests a fresh single-value container to
    // capture whatever concrete primitive `UInt8`/`UInt16`'s own
    // `encode(to:)` writes through it.
    try value.encode(to: ArrayEncoder(storage: storage))
  }
  mutating func nestedContainer<NestedKey>(
    keyedBy keyType: NestedKey.Type
  ) -> KeyedEncodingContainer<NestedKey> {
    fatalError("not implemented")
  }
  mutating func nestedUnkeyedContainer() -> any UnkeyedEncodingContainer {
    fatalError("not implemented")
  }
  mutating func superEncoder() -> any Encoder {
    fatalError("not implemented")
  }
}

private struct ArraySingleValueEncoding: SingleValueEncodingContainer {
  let storage: ArrayCoderStorage
  var codingPath: [any CodingKey] = []

  mutating func encodeNil() throws { storage.elements.append(Optional<Any>.none as Any) }
  mutating func encode(_ value: Bool) throws { fatalError("not implemented") }
  mutating func encode(_ value: String) throws { fatalError("not implemented") }
  mutating func encode(_ value: Double) throws { fatalError("not implemented") }
  mutating func encode(_ value: Float) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int8) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int16) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int32) throws { fatalError("not implemented") }
  mutating func encode(_ value: Int64) throws { fatalError("not implemented") }
  mutating func encode(_ value: UInt) throws { fatalError("not implemented") }
  mutating func encode(_ value: UInt8) throws { storage.elements.append(value) }
  mutating func encode(_ value: UInt16) throws { storage.elements.append(value) }
  mutating func encode(_ value: UInt32) throws { fatalError("not implemented") }
  mutating func encode(_ value: UInt64) throws { fatalError("not implemented") }
  mutating func encode<T: Encodable>(_ value: T) throws {
    try value.encode(to: ArrayEncoder(storage: storage))
  }
}

private struct ArrayDecoder: Decoder {
  let storage: ArrayCoderStorage
  var codingPath: [any CodingKey] = []
  var userInfo: [CodingUserInfoKey: Any] = [:]

  func container<Key>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key> {
    fatalError("not implemented")
  }
  func unkeyedContainer() throws -> any UnkeyedDecodingContainer {
    return ArrayUnkeyedDecoding(storage: storage)
  }
  func singleValueContainer() throws -> any SingleValueDecodingContainer {
    return ArraySingleValueDecoding(storage: storage)
  }
}

private struct ArrayUnkeyedDecoding: UnkeyedDecodingContainer {
  let storage: ArrayCoderStorage
  var codingPath: [any CodingKey] = []
  var count: Int? { storage.countOverride ?? storage.elements.count }
  var isAtEnd: Bool { storage.readIndex >= storage.elements.count }
  var currentIndex: Int { storage.readIndex }

  mutating func decodeNil() throws -> Bool { fatalError("not implemented") }
  mutating func decode(_ type: Bool.Type) throws -> Bool { fatalError("not implemented") }
  mutating func decode(_ type: String.Type) throws -> String { fatalError("not implemented") }
  mutating func decode(_ type: Double.Type) throws -> Double { fatalError("not implemented") }
  mutating func decode(_ type: Float.Type) throws -> Float { fatalError("not implemented") }
  mutating func decode(_ type: Int.Type) throws -> Int { fatalError("not implemented") }
  mutating func decode(_ type: Int8.Type) throws -> Int8 { fatalError("not implemented") }
  mutating func decode(_ type: Int16.Type) throws -> Int16 { fatalError("not implemented") }
  mutating func decode(_ type: Int32.Type) throws -> Int32 { fatalError("not implemented") }
  mutating func decode(_ type: Int64.Type) throws -> Int64 { fatalError("not implemented") }
  mutating func decode(_ type: UInt.Type) throws -> UInt { fatalError("not implemented") }
  mutating func decode(_ type: UInt8.Type) throws -> UInt8 { fatalError("not implemented") }
  mutating func decode(_ type: UInt16.Type) throws -> UInt16 { fatalError("not implemented") }
  mutating func decode(_ type: UInt32.Type) throws -> UInt32 { fatalError("not implemented") }
  mutating func decode(_ type: UInt64.Type) throws -> UInt64 { fatalError("not implemented") }
  mutating func decode<T: Decodable>(_ type: T.Type) throws -> T {
    // As with `ArrayUnkeyedEncoding.encode<T: Encodable>(_:)` above,
    // `UncheckedString.init(from:)` calls `container.decode(Element.self)`
    // generically over `Element: Decodable`, so this generic fallback --
    // not any of the concrete-type overloads above -- is the one that
    // actually runs.
    return try T(from: ArrayDecoder(storage: storage))
  }
  mutating func nestedContainer<NestedKey>(
    keyedBy type: NestedKey.Type
  ) throws -> KeyedDecodingContainer<NestedKey> {
    fatalError("not implemented")
  }
  mutating func nestedUnkeyedContainer() throws -> any UnkeyedDecodingContainer {
    fatalError("not implemented")
  }
  mutating func superDecoder() throws -> any Decoder {
    fatalError("not implemented")
  }
}

private struct ArraySingleValueDecoding: SingleValueDecodingContainer {
  let storage: ArrayCoderStorage
  var codingPath: [any CodingKey] = []

  func decodeNil() -> Bool { fatalError("not implemented") }
  func decode(_ type: Bool.Type) throws -> Bool { fatalError("not implemented") }
  func decode(_ type: String.Type) throws -> String { fatalError("not implemented") }
  func decode(_ type: Double.Type) throws -> Double { fatalError("not implemented") }
  func decode(_ type: Float.Type) throws -> Float { fatalError("not implemented") }
  func decode(_ type: Int.Type) throws -> Int { fatalError("not implemented") }
  func decode(_ type: Int8.Type) throws -> Int8 { fatalError("not implemented") }
  func decode(_ type: Int16.Type) throws -> Int16 { fatalError("not implemented") }
  func decode(_ type: Int32.Type) throws -> Int32 { fatalError("not implemented") }
  func decode(_ type: Int64.Type) throws -> Int64 { fatalError("not implemented") }
  func decode(_ type: UInt.Type) throws -> UInt { fatalError("not implemented") }
  func decode(_ type: UInt8.Type) throws -> UInt8 {
    defer { storage.readIndex += 1 }
    return storage.elements[storage.readIndex] as! UInt8
  }
  func decode(_ type: UInt16.Type) throws -> UInt16 {
    defer { storage.readIndex += 1 }
    return storage.elements[storage.readIndex] as! UInt16
  }
  func decode(_ type: UInt32.Type) throws -> UInt32 { fatalError("not implemented") }
  func decode(_ type: UInt64.Type) throws -> UInt64 { fatalError("not implemented") }
  func decode<T: Decodable>(_ type: T.Type) throws -> T { fatalError("not implemented") }
}

private func encodeToArray<T: Encodable>(_ value: T) throws -> [Any] {
  let storage = ArrayCoderStorage()
  try value.encode(to: ArrayEncoder(storage: storage))
  return storage.elements
}

private func decodeFromArray<T: Decodable>(
  _ type: T.Type, _ elements: [Any], countOverride: Int? = nil
) throws -> T {
  let storage = ArrayCoderStorage()
  storage.elements = elements
  storage.countOverride = countOverride
  return try T(from: ArrayDecoder(storage: storage))
}

UncheckedStringTests.test("Codable/UInt8") {
  // Non-ASCII, `\x{hh}`-escaped content round-trips as an array of raw
  // `UInt8` code units -- not as text.
  let s: UncheckedString<UInt8> = "Ren\x{e9} Descartes"
  let encoded = try! encodeToArray(s)
  expectEqual(encoded.count, s.count)
  expectTrue((encoded as! [UInt8]).elementsEqual([
    82, 101, 110, 233, 32, 68, 101, 115, 99, 97, 114, 116, 101, 115
  ]))

  let decoded = try! decodeFromArray(UncheckedString<UInt8>.self, encoded)
  expectTrue(decoded == s)
}

UncheckedStringTests.test("Codable/UInt16") {
  let s: UncheckedString<UInt16> = "hello, world!"
  let encoded = try! encodeToArray(s)
  let decoded = try! decodeFromArray(UncheckedString<UInt16>.self, encoded)
  expectTrue(decoded == s)
}

UncheckedStringTests.test("Codable/underestimatedCount") {
  // `init(from:)` reserves capacity based on the decoder's reported
  // `count`, but must not trust it for anything beyond that hint --
  // mirroring `Data.init(from:)`'s guard against an underestimate. Here
  // the container claims 2 elements but 5 are actually present; all 5
  // must still be decoded.
  let elements: [Any] = [UInt8(1), UInt8(2), UInt8(3), UInt8(4), UInt8(5)]
  let decoded = try! decodeFromArray(
    UncheckedString<UInt8>.self, elements, countOverride: 2
  )
  expectTrue(decoded == UncheckedString<UInt8>([1, 2, 3, 4, 5]))
}

UncheckedStringTests.test("Codable/subStringViaConversion") {
  // `UncheckedSubString`, like `Substring`, does not conform to `Codable`.
  // Converting to `UncheckedString` first (already free via the existing
  // `Collection`-based initializer) is the supported path.
  let s: UncheckedString<UInt8> = "Ren\x{e9} Descartes"
  let sub = s[s.startIndex..<s.index(s.startIndex, offsetBy: 3)]
  let encoded = try! encodeToArray(UncheckedString(sub))
  let decoded = try! decodeFromArray(UncheckedString<UInt8>.self, encoded)
  expectTrue(decoded == UncheckedString(sub))
}

runAllTests()
