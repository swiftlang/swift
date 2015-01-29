import Foundation

@exported import ProtoWithInitializer
@exported import TypeAndValue

public class Impl : InitProto {
  @objc public required init(int i: CInt) {}

  public func takeStruct(input: testStruct) {
    testStruct(input)
  }

  public func getEnum() -> testEnum {
    return testEnum
  }
}
