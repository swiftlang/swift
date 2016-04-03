// RUN: %target-swift-frontend -emit-ir -verify %s
class GenericClass<T> { }

protocol MyProtocol { }

class MyClass {
  func myFunction<T, O: GenericClass<T> where T: MyProtocol>(_ myArg: O) -> T {
    fatalError()
  }
}


