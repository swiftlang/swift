public protocol BestProtocol {}
public protocol GoodProtocol {
  associatedtype A
}

public struct BestStruct: BestProtocol {
  public init() {}
}

extension BestProtocol {
  @available(iOS 13.0, OSX 10.15, tvOS 13.0, watchOS 6.0, *)
  public func _bestValue<X: GoodProtocol>(_ x: X.Type, _ a: X.A) -> some BestProtocol {
    return BestStruct()
  }
}

