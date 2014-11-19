@objc public class BaseClass {}

public func getBaseClass() -> BaseClass {
  return BaseClass()
}

public func useBaseClass(_: BaseClass) {}

@objc public
protocol BaseProto {}

public func useBaseProto(_: BaseProto) {}

extension BaseClass {
  public func extensionMethod() {}
}

@objc public enum BaseEnum: CShort {
  case Zim
  case Zang
  case Zung
}

@objc public class AnotherClass {
  @objc public func getEnum() -> BaseEnum { return .Zung }
  public init() {}
}
