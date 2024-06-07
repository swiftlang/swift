// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

public protocol P {}

extension P {
  @_transparent
  public func f() {
    let x: any Q<Self> = S<Self>()

    // force a copy
    g(x, x)
  }
}

@_optimize(none)
public func g<T>(_: consuming T, _: consuming T) {}

public enum G<T> {
  case ok(String)
  case bar
}

public func h(e: any P) {
  // We inline P.f(), because it is transparent, so we end
  // up working with an `any Q<@opened ...>`.
  e.f()
}

public protocol Q<A> {
  associatedtype A
}

public struct S<A>: Q {
  public init() {}
}
