public protocol Butt {}
extension Int: Butt {}

public func exportsOpaqueReturn() -> some Butt { return 0 }
