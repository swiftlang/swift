// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {
  typealias T = Int
}

protocol P2 {
  associatedtype T
}

// CHECK: ExtensionDecl line={{.*}} base=P1
// CHECK-NEXT: Generic signature: <Self where Self : P1, Self : P2, Self.[P2]T == Int>
extension P1 where Self : P2, T == Int {
  func takeT11(_: T) {}
  func takeT12(_: Self.T) {}
}

// CHECK: ExtensionDecl line={{.*}} base=P1
// CHECK-NEXT: Generic signature: <Self where Self : P1, Self : P2, Self.[P2]T == Int>
extension P1 where Self : P2, Self.T == Int {
  func takeT21(_: T) {}
  func takeT22(_: Self.T) {}
}

// CHECK: ExtensionDecl line={{.*}} base=P1
// CHECK-NEXT: Generic signature: <Self where Self : P1, Self : P2, Self.[P2]T == Int>
extension P1 where Self : P2 {
  func takeT31(_: T) {}
  func takeT32(_: Self.T) {}
}

// Same as above, but now we have two visible associated types with the same
// name.
protocol P3 {
  associatedtype T
}

// CHECK: ExtensionDecl line={{.*}} base=P2
// CHECK-NEXT: Generic signature: <Self where Self : P2, Self : P3, Self.[P2]T == Int>
extension P2 where Self : P3, T == Int {
  func takeT41(_: T) {}
  func takeT52(_: Self.T) {}
}

extension P2 where Self : P3 {
  func takeT61(_: T) {}
  func takeT62(_: Self.T) {}
}

protocol P4 : P2, P3 {
  func takeT71(_: T)
  func takeT72(_: Self.T)
}
