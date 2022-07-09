
public protocol P {}

struct PImpl: P {}

public struct Wrapper<T: P, U>: P {
  public init(value: T, extra: U) {}
}

private struct Burrito {}

extension P {
  @inlinable
  public func wrapped<U>(extra: U) -> Wrapper<Self, U> {
    return Wrapper(value: self, extra: extra)
  }

  public func burritoed() -> some P {
    return wrapped(extra: Burrito())
  }
}

public class Butz<T: P> {
  init(_: T) {}
}


