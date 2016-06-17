// RUN: %target-parse-verify-swift -enable-experimental-nested-generic-types

struct OuterNonGeneric {
  struct MidNonGeneric {
    struct InnerNonGeneric {}
    struct InnerGeneric<A> {}
  }

  struct MidGeneric<B> {
    struct InnerNonGeneric {}
    struct InnerGeneric<C> {}

    func flock(_ b: B) {}
  }
}

struct OuterGeneric<D> {
  struct MidNonGeneric {
    struct InnerNonGeneric {}
    struct InnerGeneric<E> {}

    func roost(_ d: D) {}
  }

  struct MidGeneric<F> {
    struct InnerNonGeneric {}
    struct InnerGeneric<G> {}

    func nest(_ d: D, f: F) {}
  }

  func nonGenericMethod(_ d: D) {
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

  class InnerGenericClass<U> : OuterNonGenericClass {
    override init() {
      super.init()
    }
  }
}

class OuterGenericClass<T> {
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

  class InnerNonGenericClass2 : OuterGenericClass {
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass3 : OuterGenericClass<Int> {
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass4 : OuterGenericClass<T> {
    override init() {
      super.init()
    }
  }

  class InnerGenericClass<U> : OuterGenericClass<U> {
    override init() {
      super.init()
    }
  }
}
