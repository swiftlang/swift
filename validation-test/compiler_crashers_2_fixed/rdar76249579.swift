// RUN: %target-swift-frontend -emit-ir %s

public class Base {}

public class Derived : Base {}

public protocol P {}

public class C : P {}

public class G<X : P, Y : Base> {}

extension G where X == C, Y == Derived {
  public func foo() {}
}
