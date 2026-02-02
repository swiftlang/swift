// RUN: %target-swift-frontend -emit-ir -g %s

protocol Foo {
  associatedtype ErrorType: Error
}

extension Array: Error where Element: Error {}

class Bar<A: Foo> {
  func doSomething(with result: Result<Any, [A.ErrorType]>) {}
}
