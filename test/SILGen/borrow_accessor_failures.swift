// RUN:%target-swift-frontend -emit-silgen %s -enable-experimental-feature BorrowAndMutateAccessors -sil-verify-none -verify

// REQUIRES: swift_feature_BorrowAndMutateAccessors

public class Klass {
  var id: Int = 0
}

func get_k() -> Klass {
  return Klass()
}

public struct S {
  var _k: Klass

  var borrow_k: Klass {
    borrow {
      return _k
    }
  }
  var get_k: Klass {
    get {
      return _k
    }
  }
  var read_k: Klass {
    _read {
      yield _k
    }
  }
}

public struct Wrapper {
  var _k: Klass
  var _s: S

  var k: Klass {
    borrow {
      return _k
    }
  }

  var s: S {
    borrow {
      return _s
    }
  }

  var s_get: S {
    get {
      return _s
    }
  }

  var s_read: S {
    _read {
      yield _s
    }
  }

  var nested_get1: Klass {
    borrow {
      return _s.get_k // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var nested_get2: Klass {
    borrow {
      return s_get.borrow_k // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var nested_get3: Int {
    borrow {
      return _s.get_k.id  // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var nested_get4: Int {
    borrow {
      return s_get.borrow_k.id  // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var nested_read1: Klass {
    borrow {
      return _s.read_k // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var nested_read2: Klass {
    borrow {
      return s_read.read_k // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var owned_value_direct: Klass {
    borrow {
      return get_k() // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var owned_value_projected: Klass {
    borrow {
      let w = Wrapper(_k: Klass(), _s: S(_k: Klass()))
      return w.k // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var if_klass: Klass {
    borrow {
      if Int.random(in: 1..<100) == 0 {
        return _k
      }
      return _k // expected-error{{multiple return statements in borrow accessors are not yet supported}}
    }
  }

  var tuple_klass: (Klass, Klass) {
    borrow {
      return (_k, _k) // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var opt_klass: Klass? {
    borrow {
      return _k // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }
}

public struct SimpleWrapper<T> {
  var _prop: T

  var borrow_prop: T {
    borrow {
      return _prop
    }
  }

  var get_prop: T {
    get {
      return _prop
    }
  }

  var read_prop: T {
    _read {
      yield _prop
    }
  }
}

public struct GenWrapper<T> {
  var _prop: T
  var _w: SimpleWrapper<T>

  public var prop: T {
    borrow {
      return _prop
    }
  }

  var get_prop: T {
    borrow {
      return _w.get_prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}

    }
  }

  var read_prop: T {
    borrow {
      return _w.read_prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var if_prop: T {
    borrow {
      if Int.random(in: 1..<100) == 0 {
        return _prop
      }
      return _prop // expected-error{{multiple return statements in borrow accessors are not yet supported}}
    }
  }

  var tuple_prop: (T, T) {
    borrow {
      return (_prop, _prop) // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }

  var opt_T: T? {
    borrow {
      return _prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}

    }
  }
}

public struct SimpleNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T

  var borrow_prop: T {
    borrow {
      return _prop
    }
  }

  var get_prop: T {
    get {
      return _prop
    }
  }

  var read_prop: T {
    _read {
      yield _prop
    }
  }
}

public struct GenNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T
  var _w: SimpleNCWrapper<T>

  public var prop: T {
    borrow {
      return _prop
    }
  }

  var nested_get: T {
    borrow {
      return _w.get_prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}

    }
  }

  var nested_read: T {
    borrow {
      return _w.read_prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}

    }
  }

  var if_prop: T {
    borrow {
      if Int.random(in: 1..<100) == 0 {
        return _prop
      }
      return _prop // expected-error{{multiple return statements in borrow accessors are not yet supported}}
    }
  }

  var opt_T: T? {
    borrow {
      return _prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either stored properties or computed properties that have borrow accessors}}
    }
  }
}

