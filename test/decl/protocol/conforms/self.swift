// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype T = Int

  func hasDefault()
  func returnsSelf() -> Self
  func hasDefaultTakesT(_: T)
  func returnsSelfTakesT(_: T) -> Self

  subscript(_: T) -> Self { get }
}

extension P {
  func hasDefault() {}

  func returnsSelf() -> Self {
    return self
  }

  func hasDefaultTakesT(_: T) {}

  func returnsSelfTakesT(_: T) -> Self { // expected-note {{'returnsSelfTakesT' declared here}}
    return self
  }

  subscript(_: T) -> Self { self } // expected-note {{'subscript(_:)' declared here}}
}

// This fails
class Class : P {}
// expected-error@-1 {{method 'returnsSelfTakesT' in non-final class 'Class' cannot be implemented in a protocol extension because it returns 'Self' and has associated type requirements}}
// expected-error@-2 {{subscript 'subscript(_:)' in non-final class 'Class' cannot be implemented in a protocol extension because it returns 'Self' and has associated type requirements}}

// This succeeds, because the class is final
final class FinalClass : P {}

// This succeeds, because we're not using the default implementations
class NonFinalClass : P {
  // FIXME: An explicit type witness is necessary to avoid an unrelated
  // associated type inference bug.
  typealias T = Never

  func returnsSelfTakesT(_: T) -> Self {
    return self
  }

  subscript(_: T) -> Self { self }
}

// Test for default implementation that comes from a constrained extension
// - https://bugs.swift.org/browse/SR-7422

// FIXME: Better error message here?

class SillyClass {}

protocol HasDefault {
  func foo()
  // expected-note@-1 {{protocol requires function 'foo()' with type '() -> ()'; do you want to add a stub?}}
}

extension HasDefault where Self == SillyClass {
  func foo() {}
  // expected-note@-1 {{candidate would match if 'SillyClass' conformed to 'HasDefault'}}
}

extension SillyClass : HasDefault {}
// expected-error@-1 {{type 'SillyClass' does not conform to protocol 'HasDefault'}}

// This is OK, though
class SeriousClass {}

extension HasDefault where Self : SeriousClass {
  func foo() {}
  // expected-note@-1 {{candidate would match if 'SillyClass' subclassed 'SeriousClass'}}
}

extension SeriousClass : HasDefault {}

// https://bugs.swift.org/browse/SR-7428

protocol Node {
  associatedtype ValueType = Int

  func addChild<ChildType>(_ child: ChildType)
    where ChildType: Node, ChildType.ValueType == Self.ValueType
}

extension Node {
  func addChild<ChildType>(_ child: ChildType)
    where ChildType: Node, ChildType.ValueType == Self.ValueType {}
}

class IntNode: Node {}

// SR-8902
protocol P8902 {
    associatedtype A
    func f(_ x: A) -> Self
}
struct S : P8902 {
    func f(_ x: Bool) -> S { fatalError() }
}
class C8902 : P8902 {
    func f(_ x: Bool) -> C8902 { fatalError() } // expected-error {{method 'f' in non-final class 'C8902' must return 'Self' to conform to protocol 'P8902'}}
}
final class C8902b : P8902 {
    func f(_ x: Bool) -> C8902b { fatalError() }
}
class C8902c : P8902 {
    func f(_ x: Bool) -> Self { fatalError() }
}
protocol P8902complex {
  associatedtype A
  func f() -> (A, Self?)
}
final class C8902complex : P8902complex {
  func f() -> (Bool, C8902complex?) { fatalError() }
}

