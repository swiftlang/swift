private(set) var privateSetGlobal = 0

struct Members {
  private(set) var privateSetProp = 0
  private(set) subscript() -> Int {
    get { return 0 }
    set {}
  }
}

struct PrivateConformance : PrivateProtocol, FilePrivateProtocol {}

private protocol PrivateProtocol {}

extension PrivateProtocol {
  public func publicExtensionMember() {}
  // expected-note@-1 {{'publicExtensionMember' declared here}}

  internal func internalExtensionMember() {}
  // expected-note@-1 {{'internalExtensionMember' declared here}}
}

fileprivate protocol FilePrivateProtocol {}

extension FilePrivateProtocol {
  public func publicFPExtensionMember() {}
  // expected-note@-1 {{'publicFPExtensionMember' declared here}}

  internal func internalFPExtensionMember() {}
  // expected-note@-1 {{'internalFPExtensionMember' declared here}}
}
