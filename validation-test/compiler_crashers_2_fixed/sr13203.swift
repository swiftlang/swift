// RUN: %target-swift-frontend -emit-ir %s

protocol MyProtocol {}

func doNotCrash1() -> some MyProtocol {
  class MyClass1: MyProtocol {}
  return MyClass1()
}

var doNotCrash2: some MyProtocol {
  class MyClass2: MyProtocol {}
  return MyClass2()
}
