// RUN: %target-typecheck-verify-swift

protocol P {
    associatedtype A
}

struct S<A>: P {}

// expected-error@+1 {{same-type constraint 'Self' == 'S<Self.A>' is recursive}}
extension P where Self == S<A> {
  func f1(_: A) -> Self {}
}

// expected-error@+1 {{same-type constraint 'Self' == 'S<Self.A>' is recursive}}
extension P where Self == S<Self.A> {
  func f2(_: A) -> Self {}
}

class C<A>: P {}

// expected-note@+3 {{while resolving type 'A'}}
// expected-note@+2 {{while resolving type 'C<A>'}}
// expected-error@+1 {{extension of protocol 'P' has self-referential generic requirements}}
extension P where Self : C<A> {
  func f(_: A) -> Self {}
}

// expected-error@+1 {{superclass constraint 'Self' : 'C<Self.A>' is recursive}}
extension P where Self : C<Self.A> {
  func f(_: A) -> Self {}
}

// https://github.com/apple/swift/issues/59476
protocol Fruit {
    associatedtype Output
    var output: Output? { get }
}

struct MapFruit<F: Fruit, Output>: Fruit {
    var output: Output? { fatalError() }
}

struct Garden<F: Fruit>: Fruit {
  // expected-error@+1 {{same-type constraint 'F' == 'MapFruit<G, F.Output>' is recursive}}
  init<G: Fruit>(fruit1: G) where F == MapFruit<G, Output> { }

  // expected-error@+1 {{same-type constraint 'F' == 'MapFruit<G, F.Output>' is recursive}}
  init<G: Fruit>(fruit2: G) where F == MapFruit<G, F.Output> { }

  var output: F.Output? { fatalError() }
}

// rdar://problem/90062518
extension Slice {
  // expected-error@+1 {{same-type constraint 'Base' == 'UnsafeBufferPointer<Base.Element>' is recursive}}
  public func withMemoryRebound<T, Result>(
    to type: T.Type, _ body: (UnsafeBufferPointer<T>) throws -> Result
  ) rethrows -> Result where Base == UnsafeBufferPointer<Element> {
    let rebased = UnsafeBufferPointer<Element>(rebased: self)
    return try rebased.withMemoryRebound(to: T.self, body)
  }
}