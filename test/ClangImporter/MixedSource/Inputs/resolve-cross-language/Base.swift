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

@objc(RenamedEnum) public enum SwiftEnum: CShort {
  case Quux
  case Corge
  case Grault
}

@objc public class AnotherClass {
  @objc public func getEnum() -> BaseEnum { return .Zung }
  @objc public func getSwiftEnum() -> SwiftEnum { return .Quux }
  public init() {}
}

@objc(RenamedClass) public class SwiftClass {}

public func getSwiftClass() -> SwiftClass {
  return SwiftClass()
}
