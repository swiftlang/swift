// RUN: %target-parse-verify-swift

struct OuterNonGeneric { // ok
  struct MidNonGeneric { // ok
    struct InnerNonGeneric {} // ok
    struct InnerGeneric<A> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterNonGeneric.MidNonGeneric'}}
  }

  struct MidGeneric<B> { // expected-error{{generic type 'MidGeneric' nested in type 'OuterNonGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'OuterNonGeneric.MidGeneric'}}
    struct InnerGeneric<C> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterNonGeneric.MidGeneric'}}

    func flock(_ b: B) {}
  }
}

struct OuterGeneric<D> {
  struct MidNonGeneric { // expected-error{{type 'MidNonGeneric' nested in generic type 'OuterGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'OuterGeneric<D>.MidNonGeneric'}}
    struct InnerGeneric<E> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterGeneric<D>.MidNonGeneric'}}

    func roost(_ d: D) {}
  }

  struct MidGeneric<F> { // expected-error{{generic type 'MidGeneric' nested in type 'OuterGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'OuterGeneric<D>.MidGeneric'}}
    struct InnerGeneric<G> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterGeneric<D>.MidGeneric'}}

    func nest(_ d: D, f: F) {}
  }

  protocol InnerProtocol { // expected-error{{declaration is only valid at file scope}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: D)
  }

  func nonGenericMethod(_ d: D) {
    // FIXME: local generic functions can't capture generic parameters yet
    func genericFunction<E>(_ d: D, e: E) {}

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

  func genericFunction<T>(_ t: T) {
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
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: T)
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

  func genericFunction<U>(_ t: U) {
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

func outerGenericFunction<T>(_ t: T) {
  struct InnerNonGeneric { // expected-error{{type 'InnerNonGeneric' nested in generic function 'outerGenericFunction' }}
    func nonGenericMethod(_ t: T) {}
    func genericMethod<V where V : Racoon, V.Stripes == T>(_ t: T) -> V {}
  }

  struct InnerGeneric<U> { // expected-error{{type 'InnerGeneric' nested in generic function 'outerGenericFunction' }}
    func nonGenericMethod(_ t: T, u: U) {}
    func genericMethod<V where V : Racoon, V.Stripes == T>(_ t: T, u: U) -> V {}
  }
}

struct S1 {
  // expected-error @+4 {{type member may not be named 'Type', since it would conflict with the 'foo.Type' expression}}
  // expected-error @+3 {{type member may not be named 'Type', since it would conflict with the 'foo.Type' expression}}
  // expected-note @+2 {{if this name is unavoidable, use backticks to escape it}} {{8-12=`Type`}}
  // expected-note @+1 {{if this name is unavoidable, use backticks to escape it}} {{8-12=`Type`}}
  enum Type {
    case A
  }
}

struct S2 {
  enum `Type` {
    case A
  }
}

let s1: S1.Type = .A // expected-error{{type of expression is ambiguous without more context}}
let s2: S2.`Type` = .A // no-error
