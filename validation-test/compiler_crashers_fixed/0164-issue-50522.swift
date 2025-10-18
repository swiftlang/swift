// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/50522

protocol P1 {}
protocol P2 {}

struct Var<N> {}

extension Var : P2 where N : P1 { }

protocol P3 {}

// CHECK: ExtensionDecl line={{.*}} base=Var
// CHECK: Generic signature: <N where N : P1>
extension Var : P3 where Self : P2 {}
