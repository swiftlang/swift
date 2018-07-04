public protocol P {
  func publicRequirement()
}

@usableFromInline
protocol Q : P {
  func internalRequirement()
}

fileprivate protocol R : Q {}

extension R {
  public func publicRequirement() {}
}

extension Q {
  public func internalRequirement() {}
}

public struct S : R {}
