// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

struct G<Value> {}

protocol P {
  associatedtype T
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: <Value where Value : P, Value.[P]T : CaseIterable, Value.[P]T.[CaseIterable]AllCases : RandomAccessCollection>
extension G where Value: P, Value.T: CaseIterable, Value.T.AllCases: RandomAccessCollection {}
