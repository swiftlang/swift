// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -emit-module-path %t/out.swiftmodule
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

public struct S {
  public typealias A = Int
}

public protocol P {
  typealias A = S
}

public struct G<T> {}

public protocol Q: P {
  typealias B = G<Self.A.A>
}

// FIXME: This should be diagnosed as an error.

// CHECK-LABEL: rdar136686001.(file).f@
// CHECK-NEXT: Generic signature: <T, U where T : Q>
public func f<T: Q, U>(_: T, _: U) where T.B == U {}
