public protocol Butt {}
extension Int: Butt {}

public func exportsOpaqueReturn() -> some Butt { return 0 }

extension Int {
  public func someButt() -> some Butt {
    return self
  }
}
