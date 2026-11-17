// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct Small: P, ~Copyable {
  var tag: Int
  init(_ tag: Int) { self.tag = tag }
}

struct Unrelated: Q, ~Copyable {}

enum NCEnum: P, ~Copyable {
  case holds(Int)
  case empty
}

final class Canary {
  static var deinitCount = 0
  deinit { Canary.deinitCount += 1 }
}

struct WithCanary: P, ~Copyable {
  let canary: Canary
}

// A genuine value cast that can fail: `Unrelated` and `Small` are unrelated
// concrete types, so this exercises the direct-branch mechanism on both the
// failing and succeeding attempt.
func viaStruct(_ box: consuming Small) -> Int? {
  if case let u as Unrelated = box { _ = u; return nil }
  if case let s as Small = box { return s.tag }
  return nil
}

func viaEnum(_ box: consuming NCEnum) -> Bool {
  if case is (P & ~Copyable) = box { return true }
  return false
}

func viaGeneric<S: ~Copyable>(_ box: consuming S) -> Bool {
  if case is (P & ~Copyable) = box { return true }
  return false
}

// A failed, non-final attempt in the chain must not consume
func chainPreservesOnFailure(_ box: consuming Small) -> Int {
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let s as Small = box { return s.tag }
  return -2
}

// CHECK: struct: 42
print("struct:", viaStruct(Small(42)) ?? -1)
// CHECK: enum: true
print("enum:", viaEnum(.holds(1)))
// CHECK: generic: true
print("generic:", viaGeneric(Small(2)))
// CHECK: chain: 7
print("chain:", chainPreservesOnFailure(Small(7)))

// Exactly one deinit per value actually bound and consumed -- no leak, no
// double free, for a payload large enough to require out-of-line storage
// when it's boxed for the match.
func bindsCanaryExactlyOnce(_ box: consuming WithCanary) -> Bool {
  if case let v as (P & ~Copyable) = box { _ = v; return true }
  return false
}
_ = bindsCanaryExactlyOnce(WithCanary(canary: Canary()))
// CHECK: canary deinits: 1
print("canary deinits:", Canary.deinitCount)
