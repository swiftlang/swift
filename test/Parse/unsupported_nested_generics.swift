// RUN: %target-parse-verify-swift

struct OuterNonGeneric { // ok
  struct MidNonGeneric { // ok
    struct InnerNonGeneric {} // ok
    struct InnerGeneric<A> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterNonGeneric.MidNonGeneric'}}
  }

  struct MidGeneric<B> { // expected-error{{generic type 'MidGeneric' nested in type 'OuterNonGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'OuterNonGeneric.MidGeneric'}}
    struct InnerGeneric<C> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterNonGeneric.MidGeneric'}}
  }
}

struct OuterGeneric<D> {
  struct MidNonGeneric { // expected-error{{type 'MidNonGeneric' nested in generic type 'OuterGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'OuterGeneric<D>.MidNonGeneric'}}
    struct InnerGeneric<E> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterGeneric<D>.MidNonGeneric'}}
  }

  struct MidGeneric<F> { // expected-error{{generic type 'MidGeneric' nested in type 'OuterGeneric'}}
    struct InnerNonGeneric {} // expected-error{{type 'InnerNonGeneric' nested in generic type 'OuterGeneric<D>.MidGeneric'}}
    struct InnerGeneric<G> {} // expected-error{{generic type 'InnerGeneric' nested in type 'OuterGeneric<D>.MidGeneric'}}
  }
}
