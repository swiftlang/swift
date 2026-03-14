// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift

// Make sure we can produce a valid interface
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Lib
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Lib

// https://github.com/swiftlang/swift/issues/86285
public enum A {}

extension A {
  public struct B<T: A.P> {}
}
extension A.B {
  public struct Set {}
}
extension A.B.Set {
  public struct D {}
}
extension A.B.Set.D {
  public struct E {}
}
extension A {
  public protocol P {}
}

public enum B {}

extension B {
  public struct Set<T: A.P> {}
}
extension B.Set {
  public struct X {}
}
extension B.Set.X {
  public struct E {}
}
extension B {
  public protocol P {}
}
