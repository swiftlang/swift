// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t/foo.swiftmodule -Xfrontend -debug-generic-signatures %s 2>&1 | %FileCheck %s

public protocol P { }
public struct X<T: P> {
  public init() { }
}

// CHECK-LABEL: main.(file).Q@
// CHECK-NEXT: Requirement signature: <Self where Self.[Q]A : P, Self.[Q]C : Collection, Self.[Q]C.[Sequence]Element == X<Self.[Q]A>>
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
