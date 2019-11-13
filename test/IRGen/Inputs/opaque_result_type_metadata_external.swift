
public protocol P {
  static func stat()
}

extension Int: P {
  public static func stat() {}
}

public func external_opaque() -> some P {
  return 0
}

@inlinable
public func external_inlinable() -> some P {
  return 0
}
