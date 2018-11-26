// RUN: %target-typecheck-verify-swift -swift-version 5

class A<T> {
  var foo: Int? { return 42 }
  func baz() -> T { fatalError() }
  func fiz() -> Int { return 42 }
}

protocol P1 {
  associatedtype T
  var foo: Int? { get }
  func baz() -> T
  func fiz() -> Int
}

protocol P2 : P1 {
  var bar: Int? { get }
}

extension P2 where Self: A<Int> {
  var bar: Int? {
    guard let foo = foo else { return 0 }
    _ = foo
    let _ = baz()
    return fiz()  
  }
}
