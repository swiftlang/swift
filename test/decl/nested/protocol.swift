// RUN: %target-typecheck-verify-swift -parse-as-library

// Protocols cannot be nested inside other types, and types cannot
// be nested inside protocols

struct OuterGeneric<D> {
  protocol InnerProtocol { // expected-error{{protocol 'InnerProtocol' cannot be nested inside another declaration}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: D)
  }
}

class OuterGenericClass<T> {
  protocol InnerProtocol { // expected-error{{protocol 'InnerProtocol' cannot be nested inside another declaration}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: T)
  }
}

protocol OuterProtocol {
  associatedtype Hen
  protocol InnerProtocol { // expected-error{{protocol 'InnerProtocol' cannot be nested inside another declaration}}
  // expected-note@-1 {{did you mean 'InnerProtocol'?}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ h: Hen)
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
  }
  struct Fang<T> { // expected-error{{type 'Fang' cannot be nested in protocol 'Racoon'}}
    func gnaw(_ s: Stripes) {}
  }
  enum Fur { // expected-error{{type 'Fur' cannot be nested in protocol 'Racoon'}}
    case Stripes
  }
}

enum SillyRawEnum : SillyProtocol.InnerClass {}
// expected-error@-1 {{raw type 'SillyProtocol.InnerClass' is not expressible by any literal}}
// expected-error@-2 {{'SillyRawEnum' declares raw type 'SillyProtocol.InnerClass', but does not conform to RawRepresentable and conformance could not be synthesized}}
// expected-error@-3 {{RawRepresentable conformance cannot be synthesized because raw type 'SillyProtocol.InnerClass' is not Equatable}}

protocol SillyProtocol {
  class InnerClass<T> {} // expected-error {{type 'InnerClass' cannot be nested in protocol 'SillyProtocol'}}
}

enum OuterEnum {
  protocol C {} // expected-error{{protocol 'C' cannot be nested inside another declaration}}
  // expected-note@-1{{'C' previously declared here}}
  case C(C) // expected-error{{invalid redeclaration of 'C'}}
}

class OuterClass<T> {
  protocol InnerProtocol : OuterClass { }
  // expected-error@-1{{protocol 'InnerProtocol' cannot be nested inside another declaration}}
}
