// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

struct Value: Hashable {
  let v: Int
}

struct Pair<T: Hashable, U: Hashable>: Hashable {
  let a: T
  let b: U
}
typealias PSI = Pair<String, Int>

var StructSynthesisTests = TestSuite("StructSynthesis")

StructSynthesisTests.test("BasicEquatability/Hashability") {
  checkHashable([Value(v: 1), Value(v: 2)], equalityOracle: { $0 == $1 })
}

// Not guaranteed by the semantics of Hashable, but we sanity check that the
// synthesized hash function is good enough to not let nearby values collide.
StructSynthesisTests.test("CloseValuesDoNotCollide") {
  expectNotEqual(Value(v: 1).hashValue, Value(v: 2).hashValue)
}

StructSynthesisTests.test("GenericEquatability/Hashability") {
  checkHashable([
    PSI(a: "foo", b: 0),
    PSI(a: "bar", b: 0),
    PSI(a: "foo", b: 5),
    PSI(a: "bar", b: 5),
  ], equalityOracle: { $0 == $1 })
}

StructSynthesisTests.test("CloseGenericValuesDoNotCollide") {
  expectNotEqual(PSI(a: "foo", b: 0).hashValue, PSI(a: "goo", b: 0).hashValue)
  expectNotEqual(PSI(a: "foo", b: 0).hashValue, PSI(a: "foo", b: 1).hashValue)
  expectNotEqual(PSI(a: "foo", b: 0).hashValue, PSI(a: "goo", b: 1).hashValue)
}

func hashEncode(_ body: (inout Hasher) -> ()) -> Int {
  var hasher = Hasher()
  body(&hasher)
  return hasher.finalize()
}

// Make sure that if the user overrides the synthesized member, that one gets
// used instead.
struct Overrides: Hashable {
  let a: Int
  var hashValue: Int { return 2 }
  func hash(into hasher: inout Hasher) {
    hasher.combine(2)
  }
  static func == (lhs: Overrides, rhs: Overrides) -> Bool { return true }
}

StructSynthesisTests.test("ExplicitOverridesSynthesized") {
  checkHashable(expectedEqual: true, Overrides(a: 4), Overrides(a: 5))
  expectEqual(Overrides(a: 4).hashValue, 2)
  expectEqual(
    hashEncode { $0.combine(Overrides(a: 4)) },
    hashEncode { $0.combine(2) })
}

// ...even in an extension.
struct OverridesInExtension: Hashable {
  let a: Int
}
extension OverridesInExtension {
  var hashValue: Int { return 2 }
  func hash(into hasher: inout Hasher) {
    hasher.combine(2)
  }
  static func == (lhs: OverridesInExtension, rhs: OverridesInExtension) -> Bool { return true }
}

StructSynthesisTests.test("ExplicitOverridesSynthesizedInExtension") {
  checkHashable(expectedEqual: true, OverridesInExtension(a: 4), OverridesInExtension(a: 5))
  expectEqual(OverridesInExtension(a: 4).hashValue, 2)
  expectEqual(
    hashEncode { $0.combine(OverridesInExtension(a: 4)) },
    hashEncode { $0.combine(2) })
}

runAllTests()
