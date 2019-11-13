// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let suite = TestSuite("RawRepresentable")

extension Hasher {
  static func hash<H: Hashable>(_ value: H) -> Int {
    var hasher = Hasher()
    hasher.combine(value)
    return hasher.finalize()
  }
}



struct TrickyRawRepresentable: RawRepresentable, Hashable {
  var value: [Unicode.Scalar]

  var rawValue: String {
    return String(String.UnicodeScalarView(value))
  }

  init?(rawValue: String) {
    self.value = Array(rawValue.unicodeScalars)
  }
}

suite.test("Tricky hashing") {
  // RawRepresentable is not Equatable itself, but it does provide a generic
  // implementation of == based on rawValue. This gets picked up as the
  // implementation of Equatable.== when a concrete RawRepresentable type
  // conforms to Equatable without providing its own implementation.
  //
  // However, RawRepresentable used to not provide equivalent implementations
  // for hashing, allowing the compiler to synthesize hashing as usual, based on
  // the actual contents of the type rather than its rawValue. Thus, the
  // definitions of equality and hashing did not actually match in some cases,
  // leading to broken behavior.
  //
  // The difference between rawValue and the actual contents is subtle, and it
  // only causes problems in custom RawRepresentable implementations where the
  // rawValue isn't actually the storage representation, like the weird struct
  // above.
  //
  // rdar://problem/45308741

  let s1 = TrickyRawRepresentable(rawValue: "café")!
  let s2 = TrickyRawRepresentable(rawValue: "cafe\u{301}")!

  expectEqual(s1, s2)
  expectEqual(s1.hashValue, s2.hashValue)
  expectEqual(Hasher.hash(s1), Hasher.hash(s2))
  expectEqual(s1._rawHashValue(seed: 42), s2._rawHashValue(seed: 42))
}

struct CustomRawRepresentable: RawRepresentable, Hashable {
  var rawValue: Int

  init?(rawValue: Int) {
    self.rawValue = rawValue
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(23)
  }
}

suite.test("Custom hashing") {
  // In 5.0, RawRepresentable had a bogus default implementation for
  // _rawHashValue(seed:) that interfered with custom hashing for
  // RawRepresentable types. Adding a custom hash(into:) implementation should
  // always be enough to customize hashing.
  //
  // See https://bugs.swift.org/browse/SR-10734

  if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    let r = CustomRawRepresentable(rawValue: 42)!
    expectEqual(Hasher.hash(r), Hasher.hash(23))
  }
}

runAllTests()
