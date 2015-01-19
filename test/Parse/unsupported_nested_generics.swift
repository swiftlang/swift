// RUN: %target-parse-verify-swift

struct OuterNonGeneric { // ok
  struct MidNonGeneric { // ok
    struct InnerNonGeneric {} // ok
    struct InnerGeneric<A> {} // expected-error{{generic type 'InnerGeneric' nested in type 'MidNonGeneric'}}
  }

  struct MidGeneric<B> { // expected-error{{generic type 'MidGeneric' nested in type 'OuterNonGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'MidGeneric'}}
    struct InnerGeneric<C> {} // expected-error{{generic type 'InnerGeneric' nested in type 'MidGeneric'}}
  }
}

struct OuterGeneric<D> {
  struct MidNonGeneric { // expected-error{{type 'MidNonGeneric' nested in generic type 'OuterGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'MidNonGeneric'}}
    struct InnerGeneric<E> {} // expected-error{{generic type 'InnerGeneric' nested in type 'MidNonGeneric'}}
  }

  struct MidGeneric<F> { // expected-error{{generic type 'MidGeneric' nested in type 'OuterGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'MidGeneric'}}
    struct InnerGeneric<G> {} // expected-error{{generic type 'InnerGeneric' nested in type 'MidGeneric'}}
  }
}
