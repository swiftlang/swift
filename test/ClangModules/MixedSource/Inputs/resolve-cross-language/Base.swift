@objc class BaseClass {}

func getBaseClass() -> BaseClass {
  return BaseClass()
}

func useBaseClass(_: BaseClass) {}

@objc @class_protocol
protocol BaseProto {}

func useBaseProto(_: BaseProto) {}
