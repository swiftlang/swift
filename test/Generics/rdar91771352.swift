// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

public struct G<T: P1, U: P1> {
  // CHECK-LABEL: .f1()@
  // CHECK-NEXT: Generic signature: <T, U where T : P1, T == U>
  public func f1() where T == U {}

  // CHECK-LABEL: .f2()@
  // CHECK-NEXT: Generic signature: <T, U where T : P1, T == U>
  public func f2() where T == U, T.A.B == T {}

  // CHECK-LABEL: .f3()@
  // CHECK-NEXT: Generic signature: <T, U where T : P1, T == U>
  public func f3() where T.A.B == T, T == U {}

  // CHECK-LABEL: .f4()@
  // CHECK-NEXT: Generic signature: <T, U where T : P1, T == U>
  public func f4() where U.A.B == U, T == U {}
}

public protocol P1 {
  associatedtype A: P2 where A.B == Self
}

public protocol P2 {
  associatedtype B: P1
}
