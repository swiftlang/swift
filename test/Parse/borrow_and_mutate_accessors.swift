// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: swift_feature_CoroutineAccessors

class Klass {}

struct Wrapper {
  let _k: Klass
  var _otherK: Klass

  var k1: Klass {
    borrow {
      return _k 
    }
  }
  var k2: Klass {
    borrow { // expected-error{{variable cannot provide both a 'borrow' accessor and a getter}}
      return _k
    }
    get { // expected-note{{getter defined here}}
      return _k
    }
  }
  var k3: Klass {
    borrow { // expected-error{{variable cannot provide both a 'borrow' accessor and a '_read' accessor}}
      return _k
    }
    _read { // expected-note{{'_read' accessor defined here}}
      yield _k
    }
  }
  var k4: Klass {
    borrow { // expected-error{{variable cannot provide both a 'borrow' accessor and a 'read' accessor}}
      return _k
    }
    read { // expected-note{{'read' accessor defined here}}
      yield _k
    }
  }
  var k5: Klass {
    mutate { // expected-error{{variable with a 'mutate' accessor must also have a 'borrow' accessor, getter, addressor or 'read' accessor}}
      return &_otherK
    }
  }
  var k6: Klass {
    borrow {
      return _otherK
    }
    mutate { // expected-error{{variable cannot provide both a 'mutate' accessor and a setter}}
      return &_otherK
    }
    _modify {
      yield &_otherK
    }
    set { // expected-note{{setter defined here}}
      _otherK = newValue
    }
  }
  var k7: Klass {
    borrow {
      return _otherK
    }
    mutate { // expected-note{{mutate accessor defined here}}
      return &_otherK
    }
    unsafeMutableAddress { // expected-error{{variable cannot provide both a mutable addressor and a 'mutate' accessor}}
    }
  }
  var k8: Klass {
    borrow async throws { // expected-error{{'borrow' accessor cannot have specifier 'async'}} expected-error{{'borrow' accessor cannot have specifier 'throws'}}
      return _otherK
    }
    mutate async throws { // expected-error{{'mutate' accessor cannot have specifier 'async'}} expected-error{{'mutate' accessor cannot have specifier 'throws'}}
      return &_otherK
    }
  }
}

var i: Int

var i_accessor: Int {
  borrow { // expected-error{{a 'borrow' accessor is supported only on a struct}}
    fatalError()
  }
  mutate { // expected-error{{a 'mutate' accessor is supported only on a struct}}
    return &i // expected-error{{'&' may only be used to pass an argument to inout parameter}}
  }
}

class KlassWrapper {
  var _k: Klass

  init(_ k: Klass) {
    self._k = k
  }

  var k: Klass {
    borrow {// expected-error{{a 'borrow' accessor is supported only on a struct}}
      return _k
    }
    mutate {// expected-error{{a 'mutate' accessor is supported only on a struct}}
      return &_k
    }
  }
}

