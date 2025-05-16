// RUN: %target-swift-emit-ir -primary-file %s 2>&1 | %FileCheck %s
// RUN: %target-swift-emit-ir -primary-file %s -O 2>&1 | %FileCheck %s
// RUN: %target-swift-emit-ir -primary-file %s -Osize 2>&1 | %FileCheck %s

// CHECK-LABEL: define hidden swiftcc i1 @"$s17RangeContainsPerf08halfOpenB0ySbSnySiG_SitF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: }
func halfOpenContains(_ r: Range<Int>, _ i: Int) -> Bool {
  r.contains(i)
}

// CHECK-LABEL: define hidden swiftcc i1 @"$s17RangeContainsPerf06closedB0ySbSNySiG_SitF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: }
func closedContains(_ r: ClosedRange<Int>, _ i: Int) -> Bool {
  r.contains(i)
}

// CHECK-LABEL: define hidden swiftcc i1 @"$s17RangeContainsPerf20halfOpenPatternMatchySbSnySiG_SitF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: }
func halfOpenPatternMatch(_ r: Range<Int>, _ i: Int) -> Bool {
  r ~= i
}

// CHECK-LABEL: define hidden swiftcc i1 @"$s17RangeContainsPerf18closedPatternMatchySbSNySiG_SitF"
// CHECK-NOT: call swiftcc
// CHECK: icmp
// CHECK-NOT: call swiftcc
// CHECK-LABEL: }
func closedPatternMatch(_ r: ClosedRange<Int>, _ i: Int) -> Bool {
  r ~= i
}
