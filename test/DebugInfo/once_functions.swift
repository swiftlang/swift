// RUN: %target-swift-frontend %s -emit-ir -O -g -o - \
// RUN:   -enable-round-trip-debug-types \
// RUN:   -enable-experimental-feature OnceFunctions | %FileCheck %s

// REQUIRES: swift_feature_OnceFunctions

// After specialization, the closure parameters of `withPointer` and
// `withPointerAtMostOnce` have function types with an owned callee.

@inline(never)
func withPointer<T, R, E: Error>(
  to value: inout T, _ body: @once (UnsafeMutablePointer<T>) throws(E) -> R
) throws(E) -> R {
  try body(&value)
}

@inline(never)
func withPointerAtMostOnce<T, R, E: Error>(
  to value: inout T,
  _ body: @callable(once) (UnsafeMutablePointer<T>) throws(E) -> R
) throws(E) -> R {
  try body(&value)
}

public func test(_ x: AnyObject) -> AnyObject {
  var value = x
  return withPointer(to: &value) { _ in x }
}

public func testAtMostOnce(_ x: AnyObject) -> AnyObject {
  var value = x
  return withPointerAtMostOnce(to: &value) { _ in x }
}

// CHECK-DAG: !DICompositeType({{.*}}identifier: "$sSpyyXlGyXls5NeverOIOxyrzr_D"
// CHECK-DAG: !DICompositeType({{.*}}identifier: "$sSpyyXlGyXls5NeverOIOoxyrzr_D"
