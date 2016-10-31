// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -verify %s
class GenericClass<T> { }

protocol MyProtocol { }

class MyClass {
  func myFunction<T, O: GenericClass<T>>(myArg: O) -> T where T: MyProtocol {
    fatalError()
  }
}


