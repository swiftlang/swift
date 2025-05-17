// RUN: %target-swift-emit-ir -primary-file %s 2>&1 | %FileCheck %s
// RUN: %target-swift-emit-ir -primary-file %s -O 2>&1 | %FileCheck %s
// RUN: %target-swift-emit-ir -primary-file %s -Osize 2>&1 | %FileCheck %s


// CHECK-LABEL: define swiftcc i1 @"$s21RangeContainsInlining08halfOpenB0ySbSnySiG_SitF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: }
public func halfOpenContains(_ r: Range<Int>, _ i: Int) -> Bool {
  r.contains(i)
}

// CHECK-LABEL: define swiftcc i1 @"$s21RangeContainsInlining06closedB0ySbSNySiG_SitF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: }
public func closedContains(_ r: ClosedRange<Int>, _ i: Int) -> Bool {
  r.contains(i)
}

// CHECK-LABEL: define swiftcc i1 @"$s21RangeContainsInlining20halfOpenPatternMatchySbSnySiG_SitF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: }
public func halfOpenPatternMatch(_ r: Range<Int>, _ i: Int) -> Bool {
  r ~= i
}
