// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t/foo.swiftmodule -Xfrontend -debug-generic-signatures -Xfrontend -requirement-machine-protocol-signatures=on %s 2>&1 | %FileCheck %s

public protocol P { }
public struct X<T: P> {
  public init() { }
}

// CHECK-LABEL: main.(file).Q@
// CHECK-NEXT: Requirement signature: <Self where Self.A : P, Self.C : Collection, Self.C.Element == X<Self.A>>
public protocol Q {
  associatedtype A: P
  associatedtype C: Collection where C.Element == MyX
  typealias MyX = X<A>
}
public struct Y: P { }
extension X: Q {
  public typealias A = Y
  public typealias C = [MyX]
}
