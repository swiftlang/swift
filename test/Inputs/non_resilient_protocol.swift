public protocol NonResilientSendableBase: Sendable {
  func f()
}

public protocol NonResilientSendable: NonResilientSendableBase {
  func g()
}

public struct ConformsToNonResilientSendable: NonResilientSendable {
  public func f() { }
  public func g() { }
}
