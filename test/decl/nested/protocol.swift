// RUN: %target-typecheck-verify-swift -parse-as-library

// Protocols cannot be nested inside other types, and types cannot
// be nested inside protocols

struct OuterGeneric<D> {
  protocol InnerProtocol { // expected-error{{protocol 'InnerProtocol' cannot be nested inside another declaration}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: D) // expected-error{{use of undeclared type 'D'}}
  }
}

class OuterGenericClass<T> {
  protocol InnerProtocol { // expected-error{{protocol 'InnerProtocol' cannot be nested inside another declaration}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: T) // expected-error{{use of undeclared type 'T'}}
  }
}

protocol OuterProtocol {
  associatedtype Hen
  protocol InnerProtocol { // expected-error{{protocol 'InnerProtocol' cannot be nested inside another declaration}}
  // expected-note@-1 {{did you mean 'InnerProtocol'?}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ h: Hen) // expected-error{{use of undeclared type 'Hen'}}
  }
}

struct ConformsToOuterProtocol : OuterProtocol {
  typealias Hen = Int

  func f() { let _ = InnerProtocol.self }
  // expected-error@-1 {{use of unresolved identifier 'InnerProtocol'}}
}

protocol Racoon {
  associatedtype Stripes
  class Claw<T> { // expected-error{{type 'Claw' cannot be nested in protocol 'Racoon'}}
    func mangle(_ s: Stripes) {}
    // expected-error@-1 {{use of undeclared type 'Stripes'}}
  }
  struct Fang<T> { // expected-error{{type 'Fang' cannot be nested in protocol 'Racoon'}}
    func gnaw(_ s: Stripes) {}
    // expected-error@-1 {{use of undeclared type 'Stripes'}}
  }
  enum Fur { // expected-error{{type 'Fur' cannot be nested in protocol 'Racoon'}}
    case Stripes
  }
}

enum SillyRawEnum : SillyProtocol.InnerClass {}

protocol SillyProtocol {
  class InnerClass<T> {} // expected-error {{type 'InnerClass' cannot be nested in protocol 'SillyProtocol'}}
}

// N.B. Redeclaration checks don't see this case because `protocol A` is invalid.
enum OuterEnum {
  protocol C {} // expected-error{{protocol 'C' cannot be nested inside another declaration}}
  case C(C)
}

class OuterClass {
  protocol InnerProtocol : OuterClass { }
  // expected-error@-1{{protocol 'InnerProtocol' cannot be nested inside another declaration}}
}

class OtherGenericClass<T> {
  protocol InnerProtocol : OtherGenericClass { }
  // expected-error@-1{{protocol 'InnerProtocol' cannot be nested inside another declaration}}
  // expected-error@-2{{superclass constraint 'Self' : 'OtherGenericClass<Self>' is recursive}}
}

protocol SelfDotTest {
  func f(_: Self.Class)
  class Class {}
  // expected-error@-1{{type 'Class' cannot be nested in protocol 'SelfDotTest'}}
}

struct Outer {
  typealias E = NestedValidation.T
  protocol NestedValidation { // expected-error {{protocol 'NestedValidation' cannot be nested inside another declaration}}
    typealias T = A.B
    class A { // expected-error {{type 'A' cannot be nested in protocol 'NestedValidation'}}
      typealias B = Int
    }
  }
}

struct OuterForUFI {
  @usableFromInline
  protocol Inner { // expected-error {{protocol 'Inner' cannot be nested inside another declaration}}
    func req()
  }
}

extension OuterForUFI.Inner {
  public func extMethod() {} // The 'public' puts this in a special path.
}

func testLookup(_ x: OuterForUFI.Inner) {
  x.req()
  x.extMethod()
}

// N.B. Lookup fails here because OuterForUFI.Inner is marked invalid.
func testLookup<T: OuterForUFI.Inner>(_ x: T) {
  x.req() // expected-error {{value of type 'T' has no member 'req'}}
  x.extMethod() // expected-error {{value of type 'T' has no member 'extMethod'}}
}
