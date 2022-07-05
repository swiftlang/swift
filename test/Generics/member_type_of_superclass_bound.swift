// RUN: %target-swift-emit-silgen %s

// The substituted type of SS.x.x is computed by taking the type of S.x,
// which is T.T in the generic signature <T where T : P>, and then
// canonicalizing it in the generic signature <T : C<U>, U>.
//
// The latter generic signature does not have a conformance requirement T : P,
// but the superclass bound C<U> of T conforms to P concretely; make sure that
// the requirement machine's getCanonicalTypeInContext() can handle this.
public protocol P {
  associatedtype T
}

public class C<T> : P {}

public struct S<T : P> {
  public var x: T.T
}

public struct SS<T : C<U>, U> {
  public var x: S<T>
}


