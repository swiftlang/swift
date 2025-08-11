// RUN: %target-swift-frontend %s -emit-ir -O -disable-availability-checking -enable-experimental-feature Lifetimes | %FileCheck %s  --check-prefix=CHECK-IR

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib
// REQUIRES: CPU=arm64 || CPU=arm64e

// Check that the loop is fully optimized, including bounds check elimination and vectorization.

// CHECK-IR-LABEL: define {{.*}} @"$s24span_bounds_check_tests24loop4over5usingSis4SpanVys5UInt8VG_AFySiGtF"
// CHECK-IR:       vector.body:
// CHECK-IR:       ret
public func loop(over a: borrowing Span<UInt8>, using b: borrowing Span<Int>) -> Int {
  var result = 0
  precondition(UInt8.max < b.count)
  for i in a.indices {
    let idx = Int(a[i])
    result &+= b[idx]
  }
  return result
}

