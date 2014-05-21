@exported import ProtoWithInitializer
@exported import TypeAndValue

class Impl : InitProto {
  init(int i: CInt) {}

  func takeStruct(input: testStruct) {
    testStruct(input)
  }

  func getEnum() -> testEnum {
    return testEnum
  }
}
