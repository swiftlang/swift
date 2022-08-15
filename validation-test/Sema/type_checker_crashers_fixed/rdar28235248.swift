// RUN: not %target-swift-frontend %s -typecheck -debug-generic-signatures 2>&1 | %FileCheck %s

protocol II {
  associatedtype E
}

protocol P {
  associatedtype I : II
  associatedtype X
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=P
// CHECK-NEXT: Generic signature: <Self where Self : P, Self.[P]I.[II]E : P, Self.[P]I.[II]E.[P]X == Int>
extension P where I.E : P, I.E.X.D == Int, I.E.X == Int {}
