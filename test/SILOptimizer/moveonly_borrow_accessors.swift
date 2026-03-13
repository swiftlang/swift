// RUN: %target-swift-frontend -sil-verify-all -verify -emit-sil  -enable-experimental-feature BorrowAndMutateAccessors %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors

public struct NC : ~Copyable {}

func use<T : ~Copyable>(_ t: borrowing T) {}
func consume<T : ~Copyable>(_ t: consuming T) {}

public struct GenSimpleNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T

  var prop: T {
    borrow {
      return _prop
    }
  }
}

public struct GenNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T
  var _w: GenSimpleNCWrapper<T>

  public var prop: T {
    borrow {
      return _prop
    }
  }

  var nested1: T {
    borrow {
      return _w.prop
    }
  }

  var nested2: T {
    borrow {
      return prop
    }
  }

  subscript(index: Int) -> T {
    borrow {
      return _prop
    }
  }

  var nested_subscript: T {
    borrow {
      return self[0]
    }
  }
}

public struct SimpleNCWrapper: ~Copyable {
  var _nc: NC

  var nc: NC {
    borrow {
      return _nc
    }
  }
}

public struct NCWrapper: ~Copyable {
  var _nc: NC
  var _s: SimpleNCWrapper

  var nc: NC {
    borrow {
      return _nc
    }
  }

  var nested1: NC {
    borrow {
      return _s.nc
    }
  }

  var nested2: NC {
    borrow {
      return nc
    }
  }

  subscript(index: Int) -> NC {
    borrow {
      return _nc
    }
  }

  var nested_subscript: NC {
    borrow {
      return self[0]
    }
  }
}

func nctest() {
  let w1 = GenNCWrapper(_prop: NC(), _w: GenSimpleNCWrapper(_prop: NC()))
  use(w1.prop)
  use(w1.nested1)
  use(w1.nested2)
  use(w1._w.prop)
  consume(w1.prop) //expected-error{{'w1.prop' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  let w2 = GenNCWrapper(_prop: NC(), _w: GenSimpleNCWrapper(_prop: NC()))
  var k1 = w2.prop //expected-error{{'w2.prop' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  use(k1)
  k1 = NC()
  use(k1)

  let w3 = GenNCWrapper(_prop: NC(), _w: GenSimpleNCWrapper(_prop: NC())) //expected-error{{'w3' used after consume}}
  consume(w3) // expected-note{{consumed here}}
  use(w3.prop) // expected-note{{used here}}

  let w4 = NCWrapper(_nc: NC(), _s: SimpleNCWrapper(_nc: NC()))
  use(w4.nc)
  use(w4.nested1)
  use(w4.nested2)
  use(w4._s.nc)
  consume(w4.nc) // expected-error{{'w4.nc' is borrowed and cannot be consumed}} expected-note{{consumed here}}


  let w5 = NCWrapper(_nc: NC(), _s: SimpleNCWrapper(_nc: NC()))
  var k2 = w5.nc // expected-error{{'w5.nc' is borrowed and cannot be consumed}} expected-note{{consumed here}}
  use(k2)
  k2 = NC()
  use(k2)
}
