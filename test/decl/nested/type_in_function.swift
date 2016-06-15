// RUN: %target-parse-verify-swift -parse-as-library

// Generic class locally defined in non-generic function (rdar://problem/20116710)
func f3() {
  class B<T> {}
}

protocol Racoon {
  associatedtype Stripes
}

// Types inside generic functions -- not supported yet

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

class OuterNonGenericClass {
  func genericFunction<T>(_ t: T) {
    class InnerNonGenericClass : OuterNonGenericClass { // expected-error {{type 'InnerNonGenericClass' nested in generic function}}
      let t: T

      init(t: T) { super.init(); self.t = t }
    }

    class InnerGenericClass<U> : OuterNonGenericClass // expected-error {{type 'InnerGenericClass' nested in generic function}}
        where U : Racoon, U.Stripes == T {
      let t: T

      init(t: T) { super.init(); self.t = t }
    }
  }
}

class OuterGenericClass<T> {
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

    class InnerGenericClass<U> : OuterGenericClass<U> // expected-error {{type 'InnerGenericClass' nested in generic function}}
      where U : Racoon, U.Stripes == T {
      let t: T

      init(t: T) { super.init(); self.t = t }
    }
  }
}

