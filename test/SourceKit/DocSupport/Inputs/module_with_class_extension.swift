
public class C<T> {}
public class D {}
public class E {}

extension C where T : D {
  public func foo() {}
}

public protocol P8 {
  associatedtype T
}

extension P8 where T : D {
  public func bar() {}
}

extension P8 where T : E {
  public func baz() {}
}

extension C : P8 {}

public class F<T : D> {}
extension F : P8 {}
