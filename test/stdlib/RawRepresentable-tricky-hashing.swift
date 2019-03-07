// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// RawRepresentable is not Equatable itself, but it does provide a generic
// implementation of == based on rawValues. This gets picked up as the
// implementation of Equatable.== when a concrete RawRepresentable type conforms
// to Equatable without providing its own implementation.
//
// However, RawRepresentable used to not provide equivalent implementations for
// hashing, allowing the compiler to synthesized hashing as usual, based on the
// actual contents of the type rather than its rawValue. Thus, the definitions
// of equality and hashing may not actually match, leading to broken hashes.
//
// The difference between rawValue and the actual contents is subtle, and it
// only causes problems in custom RawRepresentable implementations where the
// rawValue isn't actually the storage representation, like the weird struct
// below.
//
// rdar://problem/45308741

struct TrickyRawRepresentable: RawRepresentable, Hashable {
  var value: [Unicode.Scalar]

  var rawValue: String {
    return String(String.UnicodeScalarView(value))
  }

  init?(rawValue: String) {
    self.value = Array(rawValue.unicodeScalars)
  }
}

let s1 = TrickyRawRepresentable(rawValue: "café")!
let s2 = TrickyRawRepresentable(rawValue: "cafe\u{301}")!

// CHECK: s1 == s2: true
print("s1 == s2: \(s1 == s2)")

// CHECK: hashValue matches: true
print("hashValue matches: \(s1.hashValue == s2.hashValue)")

extension Hasher {
  static func hash<H: Hashable>(_ value: H) -> Int {
    var hasher = Hasher()
    hasher.combine(value)
    return hasher.finalize()
  }
}

// CHECK: hash(into:) matches: true
print("hash(into:) matches: \(Hasher.hash(s1) == Hasher.hash(s2))")

// CHECK: _rawHashValue(seed:) matches: true
let r1 = s1._rawHashValue(seed: 42)
let r2 = s2._rawHashValue(seed: 42)
print("_rawHashValue(seed:) matches: \(r1 == r2)")
