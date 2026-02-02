// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -o /dev/null %s

// https://github.com/apple/swift/issues/55643

protocol MyProtocol {}

func doNotCrash1() -> some MyProtocol {
  class MyClass1: MyProtocol {}
  return MyClass1()
}

var doNotCrash2: some MyProtocol {
  class MyClass2: MyProtocol {}
  return MyClass2()
}
