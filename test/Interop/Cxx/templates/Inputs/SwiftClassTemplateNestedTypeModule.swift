import ClassTemplateNestedTypeForSwiftModule

public func receiveShipEngine(_ i: inout Ship<Bool>.Engine) {}

public func returnShipEngine() -> Ship<Bool>.Engine {
  return Ship<Bool>.Engine()
}
