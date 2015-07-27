// RUN: %target-parse-verify-swift

struct OuterNonGeneric { // ok
  struct MidNonGeneric { // ok
    struct InnerNonGeneric {} // ok
    struct InnerGeneric<A> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterNonGeneric.MidNonGeneric'}}
  }

  struct MidGeneric<B> { // expected-error{{generic type 'MidGeneric' nested in type 'OuterNonGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'OuterNonGeneric.MidGeneric'}}
    struct InnerGeneric<C> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterNonGeneric.MidGeneric'}}

    func flock(b: B) {}
  }
}

struct OuterGeneric<D> {
  struct MidNonGeneric { // expected-error{{type 'MidNonGeneric' nested in generic type 'OuterGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'OuterGeneric<D>.MidNonGeneric'}}
    struct InnerGeneric<E> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterGeneric<D>.MidNonGeneric'}}

    func roost(d: D) {}
  }

  struct MidGeneric<F> { // expected-error{{generic type 'MidGeneric' nested in type 'OuterGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'OuterGeneric<D>.MidGeneric'}}
    struct InnerGeneric<G> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterGeneric<D>.MidGeneric'}}

    func nest(d: D, f: F) {}
  }

  protocol InnerProtocol { // expected-error{{declaration is only valid at file scope}}
    typealias Rooster
    func flip(r: Rooster)
    func flop(t: D)
  }
}

class OuterNonGenericClass {
  enum InnerNonGeneric {
    case Baz
    case Zab
  } 
}

class OuterGenericClass<T> {
  enum InnerNonGeneric { // expected-error {{type 'InnerNonGeneric' nested in generic type 'OuterGenericClass' is not allowed}}
    case Baz
    case Zab
  } 

  protocol InnerProtocol { // expected-error{{declaration is only valid at file scope}}
    typealias Rooster
    func flip(r: Rooster)
    func flop(t: T)
  }

  class InnerNonGenericClass : InnerNonGenericBase {} // expected-error {{type 'InnerNonGenericClass' nested in generic type 'OuterGenericClass' is not allowed}}

  class InnerNonGenericBase {} // expected-error {{type 'InnerNonGenericBase' nested in generic type 'OuterGenericClass' is not allowed}}
}

protocol OuterProtocol {
  typealias Hen
  protocol InnerProtocol { // expected-error{{type not allowed here}}
    typealias Rooster
    func flip(r: Rooster)
    func flop(h: Hen)
  }
}

protocol Racoon {
  typealias Stripes
  class Claw<T> { // expected-error{{type not allowed here}}
    func mangle(s: Stripes) {}
  }
  struct Fang<T> { // expected-error{{type not allowed here}}
    func gnaw(s: Stripes) {}
  }
}

enum OuterEnum {
  protocol C {} // expected-error{{declaration is only valid at file scope}}
  case C(C)
}
