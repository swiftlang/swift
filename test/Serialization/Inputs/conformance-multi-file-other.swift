public class Base {
  public init() {}
}

public protocol BaseProto {
  func method()
}

extension Base : BaseProto {
  public func method() {}
}
