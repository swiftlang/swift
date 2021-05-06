public protocol P {}

public struct Underlying : P {
}

public func returnOpaque() -> some P {
  return Underlying()
}