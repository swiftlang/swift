// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -enable-library-evolution

public protocol P {
  func foo() -> Int
}
internal protocol Q: P {
  func fooImpl() -> Int32
}
extension Q {
  public func foo() -> Int { // expected-note {{mark the instance method as 'public' to satisfy the requirement}}
    return Int(self.fooImpl())
  }
}
public struct S: Q { // expected-warning {{method 'foo()' must be as accessible as its enclosing type because it matches a requirement in protocol 'P'}}
  internal func fooImpl() -> Int32 {
    return 42
  }
}
public struct Foo {
  public init(value: Int) {}
}
public protocol PublicProtocol {
  init?(integer: Int)
}
protocol InternalProtocol: PublicProtocol {}
extension InternalProtocol {
  public init(integer: Int) {} // expected-note {{mark the initializer as 'public' to satisfy the requirement}}
}
extension Foo: PublicProtocol, InternalProtocol {} // expected-warning {{initializer 'init(integer:)' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProtocol'}}
