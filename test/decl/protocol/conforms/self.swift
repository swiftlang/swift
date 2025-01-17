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

// (https://github.com/apple/swift/issues/49965) Test for default implementation
// that comes from a constrained extension.

// FIXME: Better error message here?

class SillyClass {}

protocol HasDefault {
  func foo()
  // expected-note@-1 {{protocol requires function 'foo()' with type '() -> ()'}}
}

extension HasDefault where Self == SillyClass {
  func foo() {}
  // expected-note@-1 {{candidate would match if 'SillyClass' conformed to 'HasDefault'}}
}

extension SillyClass : HasDefault {}
// expected-error@-1 {{type 'SillyClass' does not conform to protocol 'HasDefault'}}
// expected-note@-2 {{add stubs for conformance}}

// This is OK, though
class SeriousClass {}

extension HasDefault where Self : SeriousClass {
  func foo() {}
  // expected-note@-1 {{candidate would match if 'SillyClass' subclassed 'SeriousClass'}}
}

extension SeriousClass : HasDefault {}

// https://github.com/apple/swift/issues/49971

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

// https://github.com/apple/swift/issues/51408

protocol P_51408 {
    associatedtype A
    func f(_ x: A) -> Self
}
struct S : P_51408 {
    func f(_ x: Bool) -> S {}
}
class C1_51408 : P_51408 {
    func f(_ x: Bool) -> C1_51408 {} // expected-error {{method 'f' in non-final class 'C1_51408' must return 'Self' to conform to protocol 'P_51408'}}
}
final class C2_51408 : P_51408 {
    func f(_ x: Bool) -> C2_51408 {}
}
class C3_51408 : P_51408 {
    func f(_ x: Bool) -> Self {}
}

protocol P_51408_Complex {
  associatedtype A
  func f() -> (A, Self?)
}
final class C_51408_Complex : P_51408_Complex {
  func f() -> (Bool, C_51408_Complex?) {}
}

