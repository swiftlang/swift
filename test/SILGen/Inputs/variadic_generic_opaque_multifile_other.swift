public protocol P {
  func f()
}

public struct G<each T>: P {
  public func f() {}
}

public func callee<each T>(_: repeat each T) -> some P {
    G<repeat each T>()
}
