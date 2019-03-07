// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t/foo.swiftmodule %s

public protocol P { }
public struct X<T: P> {
  public init() { }
}
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
