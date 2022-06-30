import Foundation

// This input is useful to ensure the delegation status of
// an actor's initializer does not affect ABI stability

public protocol NamedEntity: Actor {
  nonisolated var name: String? { get }
}

public actor BigFoot: NamedEntity {

  public nonisolated let name: String?

  private init(withName name: String) {
    self.name = name
  }

  public init() {
  #if DELEGATES
    self.init(withName: "Sasquatch")
  #else
    self.name = nil
  #endif
  }
}


@objc public actor BigFootObjC: NSObject, NamedEntity {

  public nonisolated let name: String?

  private init(withName name: String) {
    self.name = name
  }

  @objc public override init() {
  #if DELEGATES
    self.init(withName: "Sasquatch")
  #else
    self.name = nil
  #endif
  }
}
