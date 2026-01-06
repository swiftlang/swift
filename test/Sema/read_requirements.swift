// RUN: %target-typecheck-verify-swift \
// RUN:     -verify-additional-prefix enabled- \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -debug-diagnostic-names

// REQUIRES: swift_feature_CoroutineAccessors

// A read requirement may be satisfied by
// - a stored property
// - a _read accessor
// - a read accessor
// - a get accessor
// - an unsafeAddress accessor

struct U : ~Copyable {}

protocol P : ~Copyable {
  @_borrowed
  var ubgs: U { get set }

  var urs: U { read set }

  var ur: U { read }
}

struct ImplStored : ~Copyable & P {
  var ubgs: U
  var urs: U
  var ur: U
}

struct ImplUnderscoredCoroutineAccessors : ~Copyable & P {
  typealias Property = U
  var _i: U
  var ubgs: U {
    _read {
      yield _i
    }
    _modify {
      yield &_i
    }
  }

  var urs: U {
    _read {
      yield _i
    }
    _modify {
      yield &_i
    }
  }

  var ur: U {
    _read {
      yield _i
    }
  }
}

struct ImplCoroutineAccessors : ~Copyable & P {
  var _i: U
  var ubgs: U {
    read {
      yield _i
    }
    modify {
      yield &_i
    }
  }

  var urs: U {
    read {
      yield _i
    }
    modify {
      yield &_i
    }
  }

  var ur: U {
    read {
      yield _i
    }
  }
}

struct ImplGetSet : P {
  var _i: U {
    get { return U() }
    set {}
  }
  var ubgs: U {
    get {
      return _i
    }
    set {
      _i = newValue
    }
  }

  var urs: U {
    get {
      return _i
    }
    set {
      _i = newValue
    }
  }

  var ur: U {
    get {
      return _i
    }
    set {
      _i = newValue
    }
  }
}

struct ImplUnsafeAddressors : P {
  var iAddr: UnsafePointer<U>
  var iMutableAddr: UnsafeMutablePointer<U> {
    .init(mutating: iAddr)
  }
  var ubgs: U {
    unsafeAddress {
      return iAddr
    }
    unsafeMutableAddress {
      return iMutableAddr
    }
  }

  var urs: U {
    unsafeAddress {
      return iAddr
    }
    unsafeMutableAddress {
      return iMutableAddr
    }
  }

  var ur: U {
    unsafeAddress {
      return iAddr
    }
    unsafeMutableAddress {
      return iMutableAddr
    }
  }
}
