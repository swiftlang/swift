// RUN: %target-swift-emit-ir -primary-file %s 2>&1 | %FileCheck %s
// RUN: %target-swift-emit-ir -primary-file %s -O 2>&1 | %FileCheck %s
// RUN: %target-swift-emit-ir -primary-file %s -Osize 2>&1 | %FileCheck %s

// We expect calls to `Range.contains`, `ClosedRange.contains` and `~=` over a
// `Range` instance to result in direct bound comparisons in all compilation
// modes, including unoptimized builds. (These are often used to implement
// bounds checking.)
//
// The sample functions below use bounds of different integer types to avoid
// them tail-calling each other.

// CHECK-LABEL: define swiftcc i1 @"$s21RangeContainsInlining08halfOpenB0ySbSnys4Int8VG_ADtF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: {{^}}}
public func halfOpenContains(_ r: Range<Int8>, _ i: Int8) -> Bool {
  r.contains(i)
}

// CHECK-LABEL: define swiftcc i1 @"$s21RangeContainsInlining06closedB0ySbSNys5Int16VG_ADtF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: {{^}}}
public func closedContains(_ r: ClosedRange<Int16>, _ i: Int16) -> Bool {
  r.contains(i)
}

// CHECK-LABEL: define swiftcc i1 @"$s21RangeContainsInlining20halfOpenPatternMatchySbSnys5Int32VG_ADtF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: {{^}}}
public func halfOpenPatternMatch(_ r: Range<Int32>, _ i: Int32) -> Bool {
  r ~= i
}
