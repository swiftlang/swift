// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/57264

protocol P {
  associatedtype A
  associatedtype AS: Q where AS.B == A
}

protocol Q {
  associatedtype B
}

struct S1<T : P> where T.AS.B == T.A {}
// CHECK-LABEL: .S1@
// CHECK-NEXT: Generic signature: <T where T : P>

struct S2<T: P> {
  struct Nested where T.AS.B == T.A {}
  // CHECK-LABEL: .S2.Nested@
  // CHECK-NEXT: Generic signature: <T where T : P>
}

extension S2 where T.AS.B == T.A {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=S2
// CHECK-NEXT: Generic signature: <T where T : P>

extension P where AS.B == A {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=P
// CHECK-NEXT: Generic signature: <Self where Self : P>

extension P where Self : P {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=P
// CHECK-NEXT: Generic signature: <Self where Self : P>
