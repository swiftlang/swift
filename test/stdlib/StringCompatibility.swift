// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-run %t/a.out4

// REQUIRES: executable_test

import StdlibUnittest

//===--- MyString ---------------------------------------------------------===//
/// A simple StringProtocol with a wacky .description that proves
/// LosslessStringConvertible is not infecting ordinary constructions by using
/// .description as the content of a copied string.
struct MyString {
  var base: String
}

extension MyString : BidirectionalCollection {
  typealias Iterator = String.Iterator
  typealias Index = String.Index
  typealias SubSequence = MyString
  func makeIterator() -> Iterator { return base.makeIterator() }
  var startIndex: String.Index { return base.startIndex }
  var endIndex: String.Index { return base.startIndex }
  subscript(i: Index) -> Character { return base[i] }
  subscript(indices: Range<Index>) -> MyString {
    return MyString(base: String(self.base[indices]))
  }
  func index(after i: Index) -> Index { return base.index(after: i) }
  func index(before i: Index) -> Index { return base.index(before: i) }
  func index(_ i: Index, offsetBy n: Int) -> Index {
    return base.index(i, offsetBy: n)
  }
  func distance(from i: Index, to j: Index) -> Int {
    return base.distance(from: i, to: j)
  }
}

extension MyString : RangeReplaceableCollection {
  init() { base = "" }
  mutating func append<S: Sequence>(contentsOf s: S)
  where S.Element == Character {
    base.append(contentsOf: s)
  }
  mutating func replaceSubrange<C: Collection>(_ r: Range<Index>, with c: C)
  where C.Element == Character {
    base.replaceSubrange(r, with: c)
  }
}

extension MyString : CustomStringConvertible {
  var description: String { return "***MyString***" }
}

extension MyString : TextOutputStream {
  public mutating func write(_ other: String) {
    append(contentsOf: other)
  }
}

extension MyString : TextOutputStreamable {
  public func write<Target : TextOutputStream>(to target: inout Target) {
    target.write(base)
  }
}

extension MyString : ExpressibleByUnicodeScalarLiteral {
  public init(unicodeScalarLiteral value: String) {
    base = .init(unicodeScalarLiteral: value)
  }
}
extension MyString : ExpressibleByExtendedGraphemeClusterLiteral {
  public init(extendedGraphemeClusterLiteral value: String) {
    base = .init(extendedGraphemeClusterLiteral: value)
  }
}

extension MyString : ExpressibleByStringLiteral {
  public init(stringLiteral value: String) {
    base = .init(stringLiteral: value)
  }
}

extension MyString : CustomReflectable {
  public var customMirror: Mirror {
    return base.customMirror
  }
}

extension MyString : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return base.customPlaygroundQuickLook
  }
}

extension MyString : CustomDebugStringConvertible {
  public var debugDescription: String {
    return "(***MyString***)"
  }
}

extension MyString : Equatable {
  public static func ==(lhs: MyString, rhs: MyString) -> Bool {
    return lhs.base == rhs.base
  }
}

extension MyString : Comparable {
  public static func <(lhs: MyString, rhs: MyString) -> Bool {
    return lhs.base < rhs.base
  }
}

extension MyString : Hashable {
  public var hashValue : Int {
    return base.hashValue
  }
}

extension MyString {
  public func hasPrefix(_ prefix: String) -> Bool {
    return self.base.hasPrefix(prefix)
  }

  public func hasSuffix(_ suffix: String) -> Bool {
    return self.base.hasSuffix(suffix)
  }
}

extension MyString : StringProtocol {
  typealias UTF8Index = String.UTF8Index
  typealias UTF16Index = String.UTF16Index
  typealias UnicodeScalarIndex = String.UnicodeScalarIndex
  var utf8: String.UTF8View { return base.utf8 }
  var utf16: String.UTF16View { return base.utf16 }
  var unicodeScalars: String.UnicodeScalarView { return base.unicodeScalars }
  var characters: String.CharacterView { return base.characters }
  func lowercased() -> String {
    return base.lowercased()
  }
  func uppercased() -> String {
    return base.uppercased()
  }

  init<C: Collection, Encoding: Unicode.Encoding>(
    decoding codeUnits: C, as sourceEncoding: Encoding.Type
  )
  where C.Iterator.Element == Encoding.CodeUnit {
    base = .init(decoding: codeUnits, as: sourceEncoding)
  }

  init(cString nullTerminatedUTF8: UnsafePointer<CChar>) {
    base = .init(cString: nullTerminatedUTF8)
  }
  
  init<Encoding: Unicode.Encoding>(
    decodingCString nullTerminatedCodeUnits: UnsafePointer<Encoding.CodeUnit>,
    as sourceEncoding: Encoding.Type) {
    base = .init(decodingCString: nullTerminatedCodeUnits, as: sourceEncoding)
  }
  
  func withCString<Result>(
    _ body: (UnsafePointer<CChar>) throws -> Result) rethrows -> Result {
    return try base.withCString(body)
  }

  func withCString<Result, Encoding: Unicode.Encoding>(
    encodedAs targetEncoding: Encoding.Type,
    _ body: (UnsafePointer<Encoding.CodeUnit>) throws -> Result
  ) rethrows -> Result {
    return try base.withCString(encodedAs: targetEncoding, body)
  }
}
//===----------------------------------------------------------------------===//

public typealias ExpectedConcreteSlice = Substring
public typealias ExpectedStringFromString = String
let swift = 4

var Tests = TestSuite("StringCompatibility")

Tests.test("String/Range/Slice/ExpectedType/\(swift)") {
  var s = "hello world"
  var sub = s[s.startIndex ..< s.endIndex]
  var subsub = sub[s.startIndex ..< s.endIndex]

  expectType(String.self, &s)
  expectType(ExpectedConcreteSlice.self, &sub)
  expectType(ExpectedConcreteSlice.self, &subsub)
}

Tests.test("String/ClosedRange/Slice/ExpectedType/\(swift)") {
  var s = "hello world"
  let lastIndex = s.index(before:s.endIndex)
  var sub = s[s.startIndex ... lastIndex]
  var subsub = sub[s.startIndex ... lastIndex]

  expectType(String.self, &s)
  expectType(ExpectedConcreteSlice.self, &sub)
  expectType(ExpectedConcreteSlice.self, &subsub)
}

Tests.test("Substring/Range/Slice/ExpectedType/\(swift)") {
  let s = "hello world" as Substring
  var sub = s[s.startIndex ..< s.endIndex]
  var subsub = sub[s.startIndex ..< s.endIndex]

  // slicing a String in Swift 3 produces a String
  // but slicing a Substring should still produce a Substring
  expectType(Substring.self, &sub)
  expectType(Substring.self, &subsub)
}

Tests.test("Substring/ClosedRange/Slice/ExpectedType/\(swift)") {
  let s = "hello world" as Substring
  let lastIndex = s.index(before:s.endIndex)
  var sub = s[s.startIndex ... lastIndex]
  var subsub = sub[s.startIndex ... lastIndex]

  expectType(ExpectedConcreteSlice.self, &sub)
  expectType(ExpectedConcreteSlice.self, &subsub)
}

Tests.test("RangeReplaceable.init/generic/\(swift)") {
  func check<
    T: RangeReplaceableCollection, S: Collection
  >(_: T.Type, from source: S)
  where T.Element : Equatable, T.Element == S.Element
  {
    var r = T(source)
    expectType(T.self, &r)
    expectEqualSequence(Array(source), Array(r))
  }

  check(String.self, from: "a" as String)
  check(Substring.self, from: "b" as String)
  // FIXME: Why isn't this working?
  // check(MyString.self, from: "c" as String)
  
  check(String.self, from: "d" as Substring)
  check(Substring.self, from: "e" as Substring)
  // FIXME: Why isn't this working?
  // check(MyString.self, from: "f" as Substring)

  // FIXME: Why isn't this working?
  // check(String.self, from: "g" as MyString)
  // check(Substring.self, from: "h" as MyString)
  check(MyString.self, from: "i" as MyString)
}

Tests.test("String.init(someString)/default type/\(swift)") {
  var s = String("" as String)
  expectType(ExpectedStringFromString.self, &s)
}

Tests.test("Substring.init(someString)/default type/\(swift)") {
  var s = Substring("" as Substring)
  expectType(Substring.self, &s)
}

Tests.test("LosslessStringConvertible/generic/\(swift)") {
  func f<T : LosslessStringConvertible>(_ x: T.Type) {
    _ = T("")! // unwrapping optional should work in generic context
  }
  f(String.self)
}

public typealias ExpectedUTF8ViewSlice = String.UTF8View.SubSequence

Tests.test("UTF8ViewSlicing") {
  let s = "Hello, String.UTF8View slicing world!".utf8
  var slice = s[s.startIndex..<s.endIndex]
  expectType(ExpectedUTF8ViewSlice.self, &slice)
  _ = s[s.startIndex..<s.endIndex] as String.UTF8View.SubSequence
}

runAllTests()
