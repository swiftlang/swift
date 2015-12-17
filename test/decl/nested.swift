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

  func nonGenericMethod(d: D) {
    // FIXME: local generic functions can't capture generic parameters yet
    func genericFunction<E>(d: D, e: E) {}

    genericFunction(d, e: ())
  }
}

class OuterNonGenericClass {
  enum InnerNonGeneric {
    case Baz
    case Zab
  } 

  class InnerNonGenericBase {
    init() {}
  }

  class InnerNonGenericClass1 : InnerNonGenericBase {
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass2 : OuterNonGenericClass {
    override init() {
      super.init()
    }
  }

  class InnerGenericClass<U> : OuterNonGenericClass { // expected-error {{generic type 'InnerGenericClass' nested in type 'OuterNonGenericClass'}}
    override init() {
      super.init()
    }
  }

  func genericFunction<T>(t: T) {
    class InnerNonGenericClass : OuterNonGenericClass { // expected-error {{type 'InnerNonGenericClass' nested in generic function}}
      let t: T

      init(t: T) { super.init(); self.t = t }
    }

    class InnerGenericClass<U where U : Racoon, U.Stripes == T> : OuterNonGenericClass { // expected-error {{type 'InnerGenericClass' nested in generic function}}
      let t: T

      init(t: T) { super.init(); self.t = t }
    }
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

  class InnerNonGenericBase { // expected-error {{type 'InnerNonGenericBase' nested in generic type 'OuterGenericClass' is not allowed}}
    init() {}
  }

  class InnerNonGenericClass1 : InnerNonGenericBase { // expected-error {{type 'InnerNonGenericClass1' nested in generic type 'OuterGenericClass' is not allowed}}
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass2 : OuterGenericClass { // expected-error {{type 'InnerNonGenericClass2' nested in generic type 'OuterGenericClass' is not allowed}}
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass3 : OuterGenericClass<Int> { // expected-error {{type 'InnerNonGenericClass3' nested in generic type 'OuterGenericClass' is not allowed}}
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass4 : OuterGenericClass<T> { // expected-error {{type 'InnerNonGenericClass4' nested in generic type 'OuterGenericClass' is not allowed}}
    override init() {
      super.init()
    }
  }

  class InnerGenericClass<U> : OuterGenericClass<U> { // expected-error {{type 'InnerGenericClass' nested in type 'OuterGenericClass' is not allowed}}
    override init() {
      super.init()
    }
  }

  func genericFunction<U>(t: U) {
    class InnerNonGenericClass1 : OuterGenericClass { // expected-error {{type 'InnerNonGenericClass1' nested in generic function}}
      let t: T

      init(t: T) { super.init(); self.t = t }
    }

    class InnerNonGenericClass2 : OuterGenericClass<Int> { // expected-error {{type 'InnerNonGenericClass2' nested in generic function}}
      let t: T

      init(t: T) { super.init(); self.t = t }
    }

    class InnerNonGenericClass3 : OuterGenericClass<T> { // expected-error {{type 'InnerNonGenericClass3' nested in generic function}}
      let t: T

      init(t: T) { super.init(); self.t = t }
    }

    class InnerGenericClass<U where U : Racoon, U.Stripes == T> : OuterGenericClass<U> { // expected-error {{type 'InnerGenericClass' nested in generic function}}
      let t: T

      init(t: T) { super.init(); self.t = t }
    }
  }
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

func outerGenericFunction<T>(t: T) {
  struct InnerNonGeneric { // expected-error{{type 'InnerNonGeneric' nested in generic function 'outerGenericFunction' }}
    func nonGenericMethod(t: T) {}
    func genericMethod<V where V : Racoon, V.Stripes == T>(t: T) -> V {}
  }

  struct InnerGeneric<U> { // expected-error{{type 'InnerGeneric' nested in generic function 'outerGenericFunction' }}
    func nonGenericMethod(t: T, u: U) {}
    func genericMethod<V where V : Racoon, V.Stripes == T>(t: T, u: U) -> V {}
  }
}
