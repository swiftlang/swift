// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK-LABEL: .P@
// CHECK-NEXT: Requirement signature: <Self>
protocol P {
  typealias T = Int
  associatedtype T
}
