// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P {}

struct S<T : P> {}

struct G<A, B> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <A, B where A == S<B>, B : P>
extension G where A == S<B>, B : P {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <A, B where A == S<B>, B : P>
extension G where B : P, A == S<B> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <A, B where A == S<B>, B : P>
extension G where B : P, A == S<B>, B : P {}
