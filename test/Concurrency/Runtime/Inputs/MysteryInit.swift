
// This input is useful to ensure the delegation status of
// an actor's initializer does not affect ABI stability
public actor BigFoot {

  public let name: String

  private init(withName name: String) {
    self.name = name
  }

  public init?() {
  #if DELEGATES
    self.init(withName: "Sasquatch")
  #else
    return nil
  #endif
  }
}
