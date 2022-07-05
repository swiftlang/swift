// RUN: %target-swift-emit-silgen %s

public protocol Q {}

public protocol P {}
public class C : P {}

public class G<T : P> {}

extension Q where Self : C {
  public func foo(_: (G<Self>) -> ()) {}
}
