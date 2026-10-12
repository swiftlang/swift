// RUN: %target-swift-frontend -parse-as-library -O -swift-version 5 -module-name=test %s -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// Chained lazy filters are fused into a single LazyFilterSequence only in
// Swift 5 mode (in Swift 4 mode they nest), so the language mode is explicit.
// Check that the fusing `filter` overload and both predicates are inlined into
// the loop, leaving no call to the overload and no closure call per element.

// CHECK-LABEL: sil {{.*}}@$s4test16sumFilteredRangeyS2iF :
// CHECK-NOT:     function_ref @$ss18LazyFilterSequenceV6filteryAByxGSb7ElementQzcF :
// CHECK-NOT:     apply
// CHECK:       } // end sil function '$s4test16sumFilteredRangeyS2iF'
public func sumFilteredRange(_ upperBound: Int) -> Int {
  return (0..<upperBound).lazy
    .filter { $0 % 2 == 0 }
    .filter { $0 % 3 == 0 }
    .reduce(0, &+)
}
