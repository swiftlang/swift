@objc @public class BaseClass {}

@public func getBaseClass() -> BaseClass {
  return BaseClass()
}

@public func useBaseClass(_: BaseClass) {}

@objc @class_protocol @public
protocol BaseProto {}

@public func useBaseProto(_: BaseProto) {}

extension BaseClass {
  @public func extensionMethod() {}
}
