private(set) var privateSetGlobal = 0

struct Members {
  private(set) var privateSetProp = 0
  private(set) subscript() -> Int {
    get { return 0 }
    set {}
  }
}

struct PrivateConformance : PrivateProtocol {}

private protocol PrivateProtocol {}

extension PrivateProtocol {
  public func publicExtensionMember() {}
  // expected-note@-1 {{'publicExtensionMember' declared here}}

  internal func internalExtensionMember() {}
  // expected-note@-1 {{'internalExtensionMember' declared here}}
}
