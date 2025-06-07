public protocol P {}

public struct G<T, V> : P {

  var t: T
  var v: V

  public init(_ t: T, _ v: V) {
    self.t = t
    self.v = v
  }
}

struct I {
  public init() {
  }
}

public struct E : P {
  public init() {}

  @inlinable
  public static var a : some P {
    return G(E(), C().b())
  }
}

public struct C {
  public init() {}

  @usableFromInline
  internal func b() -> some P {
    return G(self, I())
  }
}

