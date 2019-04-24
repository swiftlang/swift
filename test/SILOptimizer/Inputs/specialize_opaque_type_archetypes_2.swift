public protocol ExternalP {
  func myValue2() -> Int64
}

extension Int64 : ExternalP {
  public func myValue2() -> Int64 {
    return self + 2
  }
}

public func external() -> some ExternalP {
  return Int64(5)
}
