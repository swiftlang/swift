// RUN: %target-parse-verify-swift -parse-as-library

// Protocols cannot be nested inside other types, and types cannot
// be nested inside protocols

struct OuterGeneric<D> {
  protocol InnerProtocol { // expected-error{{declaration is only valid at file scope}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: D)
  }
}

class OuterGenericClass<T> {
  protocol InnerProtocol { // expected-error{{declaration is only valid at file scope}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: T)
  }
}

protocol OuterProtocol {
  associatedtype Hen
  protocol InnerProtocol { // expected-error{{type not allowed here}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ h: Hen)
  }
}

protocol Racoon {
  associatedtype Stripes
  class Claw<T> { // expected-error{{type not allowed here}}
    func mangle(_ s: Stripes) {}
  }
  struct Fang<T> { // expected-error{{type not allowed here}}
    func gnaw(_ s: Stripes) {}
  }
}

enum OuterEnum {
  protocol C {} // expected-error{{declaration is only valid at file scope}}
  case C(C)
}

