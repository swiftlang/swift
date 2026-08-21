// `AnyKeyPath`'s `Hashable` conformance in Embedded Swift.
//
// Split out of `keypaths-exec.swift` because using an `AnyKeyPath` as a
// `Dictionary` key pulls in `_HashTable` and the hash-seed initializer, which
// reference `ceil` and `arc4random_buf`. On Linux this link recipe provides
// neither — Ubuntu 22.04's glibc predates `arc4random_buf`, and libm isn't
// linked — so like `dict-init.swift` this test restricts itself rather than
// costing `keypaths-exec.swift` its Linux coverage.

// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -wmo -runtime-compatibility-version none %target-embedded-posix-shim) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// Embedded key paths and SIL opaque values don't currently mix: the
// combination trips `getSILArgumentConvention`. `keypaths-static.swift` and
// `keypaths-exec.swift` carry the same XFAIL.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

struct Erasable {
  var x: Int32
  var y: Int32
}
struct NestedErasable {
  var e: Erasable
}

do {
  let e = Erasable(x: 42, y: 99)
  let anyX: AnyKeyPath = \Erasable.x
  let anyY: AnyKeyPath = \Erasable.y
  let anyNested: AnyKeyPath = \NestedErasable.e.x

  // The static-instance emitter shares one immortal global per pattern, so
  // equal-shaped key paths hash and compare equal by object identity.
  var counts: [AnyKeyPath: Int] = [:]
  counts[anyX, default: 0] += 1
  counts[anyX, default: 0] += 1
  counts[anyNested, default: 0] += 1
  print(counts[anyX] == 2 && counts[anyNested] == 1 ? "OK!" : "FAIL") // CHECK: OK!

  // Distinct paths must land in distinct buckets.
  counts[anyY, default: 0] += 1
  print(counts.count == 3 ? "OK!" : "FAIL") // CHECK-NEXT: OK!

  // A freshly written key path of the same shape must hash equal to the one
  // already in the table.
  let anyXAgain: AnyKeyPath = \Erasable.x
  print(counts[anyXAgain] == 2 ? "OK!" : "FAIL") // CHECK-NEXT: OK!

  // Set membership goes through the same conformance.
  let set: Set<AnyKeyPath> = [anyX, anyY, anyNested]
  print(set.contains(anyXAgain) ? "OK!" : "FAIL") // CHECK-NEXT: OK!
  print(set.count == 3 ? "OK!" : "FAIL") // CHECK-NEXT: OK!

  // Sanity check that the values are still projectable after round-tripping
  // through the hashed containers.
  print((e[keyPath: anyX] as? Int32) == 42 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
}
