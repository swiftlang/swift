// RUN: %target-swift-frontend %s -module-name range_assume_onone -emit-sil -Onone | %FileCheck %s

// Reading a bound doesn't leave a copy of the range in a stack location for the bound's assume.

// CHECK-LABEL: sil @$s18range_assume_onone10lowerBoundySiSnySiGF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s18range_assume_onone10lowerBoundySiSnySiGF'
public func lowerBound(_ r: Range<Int>) -> Int {
  return r.lowerBound
}

// The assume of other bound types has no effect and is removed.
// CHECK-LABEL: sil @$s18range_assume_onone15indexLowerBoundySS5IndexVSnyADGF :
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     builtin "assumeTrue"
// CHECK:       } // end sil function '$s18range_assume_onone15indexLowerBoundySS5IndexVSnyADGF'
public func indexLowerBound(_ r: Range<String.Index>) -> String.Index {
  return r.lowerBound
}
