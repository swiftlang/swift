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
