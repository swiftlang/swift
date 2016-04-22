// RUN: %target-swift-frontend -emit-ir -disable-incremental-llvm-codegen -verify %s
class GenericClass<T> { }

protocol MyProtocol { }

class MyClass {
  func myFunction<T, O: GenericClass<T> where T: MyProtocol>(myArg: O) -> T {
    fatalError()
  }
}


