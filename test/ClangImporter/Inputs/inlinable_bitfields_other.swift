import ctypes

public struct MM {
  public var m: ModRM
}

@inlinable public func f(_ m: MM) -> UInt32 {
  return m.m.rm
}
