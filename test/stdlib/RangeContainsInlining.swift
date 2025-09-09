// RUN: %target-swift-emit-ir -primary-file %s 2>&1 | %FileCheck %s
// RUN: %target-swift-emit-ir -primary-file %s -O 2>&1 | %FileCheck --check-prefixes CHECK,CHECK-OPTIMIZED %s
// RUN: %target-swift-emit-ir -primary-file %s -Osize 2>&1 | %FileCheck --check-prefixes CHECK,CHECK-OPTIMIZED %s

// We expect calls to `Range.contains` to result in direct bound comparisons in
// all compilation modes, including unoptimized builds. (These are often used to
// implement bounds checking.)
//
// The sample functions below use bounds of different integer types to avoid
// them tail-calling each other.

// CHECK-LABEL: define {{.*}} i1 @"$s21RangeContainsInlining08halfOpenB0ySbSnys4Int8VG_ADtF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: {{^}}}
public func halfOpenContains(_ r: Range<Int8>, _ i: Int8) -> Bool {
  r.contains(i)
}

// `ClosedRange.contains is only marked `@inline(__always)`; unfortunately it
// doesn't get inlined in deug builds.

// CHECK-OPTIMIZED-LABEL: define  {{.*}} i1 @"$s21RangeContainsInlining06closedB0ySbSNys5Int16VG_ADtF"
// CHECK-OPTIMIZED-NOT: call swiftcc
// CHECK-OPTIMIZED: icmp
// CHECK-OPTIMIZED-NOT: call swiftcc
// CHECK-OPTIMIZED-LABEL: {{^}}}
public func closedContains(_ r: ClosedRange<Int16>, _ i: Int16) -> Bool {
  r.contains(i)
}

// `Range.~=` is only marked `@inline(__always)`; unfortunately it doesn't get
// inlined in deug builds.

// CHECK-OPTIMIZED-LABEL: define {{.*}} i1 @"$s21RangeContainsInlining20halfOpenPatternMatchySbSnys5Int32VG_ADtF"
// CHECK-OPTIMIZED-NOT: call swiftcc
// CHECK-OPTIMIZED: icmp
// CHECK-OPTIMIZED-NOT: call swiftcc
// CHECK-OPTIMIZED-LABEL: {{^}}}
public func halfOpenPatternMatch(_ r: Range<Int32>, _ i: Int32) -> Bool {
  r ~= i
}
