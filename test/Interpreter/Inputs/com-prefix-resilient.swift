@com(interface: "30000000-0000-0000-0000-000000000001")
public protocol IResilient {
  func value(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com
public final class ResilientCOMObject: IResilient {
  public let value: Int32 = 3

  public init() {
  }

  public func value(_ result: UnsafeMutablePointer<Int32>?) -> Int32 {
    result?.pointee = value
    return 0
  }
}
