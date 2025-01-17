// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol Saturable: Comparable {
  func saturated(max: Self) -> Self
}

extension Int: Saturable {
  func saturated(max: Int) -> Int {
    return self > max ? max : self
  }
}

// CHECK-NOT: sil_witness_table Int: Equatable module witnesses_refinement { 
// CHECK-NOT: sil_witness_table Int: Comparable module witnesses_refinement { 
// CHECK: sil_witness_table hidden Int: Saturable module witnesses_refinement { 

protocol P { }

protocol P0 {
  associatedtype A
}

protocol P1 {
  associatedtype A
}

protocol P2: P0 {
  associatedtype A
}

protocol P3: P2, P1 {
  associatedtype A: P
}

struct ConformsToP: P { }

// CHECK-LABEL: sil_witness_table hidden ConformsToP3: P3
// CHECK:         base_protocol P1
// CHECK-NEXT:    base_protocol P2
// CHECK-NEXT:    associated_conformance (A: P)
// CHECK-NEXT:   }
struct ConformsToP3: P3 {
  typealias A = ConformsToP
}

// CHECK-LABEL: sil_witness_table hidden ConformsToP3: P2
// CHECK:         base_protocol P0
// CHECK-NEXT:   }

// CHECK-LABEL: sil_witness_table hidden ConformsToP3: P1
// CHECK:         associated_type A: ConformsToP
// CHECK-NEXT:   }

// CHECK-LABEL: sil_witness_table hidden ConformsToP3: P0
// CHECK:         associated_type A: ConformsToP
// CHECK-NEXT:   }
