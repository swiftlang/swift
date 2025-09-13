// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: swift_feature_CoroutineAccessors

class Klass {
  var _i: Int = 0
}

struct Struct {
  var _i: Int = 0
}

extension Klass {
  var i: Int {
    borrow { // expected-error{{a borrow accessor is supported only on a struct or enum}}
      return 0
    }
    mutate { // expected-error{{a mutate accessor is supported only on a struct or enum}}
      return &_i
    }
  }
}

extension Struct {
  var i: Int {
    borrow {
      return 0
    }
    mutate {
      return &_i
    }
  }
}

// TODO: borrow and mutate protocol requirements
protocol P {
  var name: String { borrow } // expected-error{{property in protocol must have explicit { get } or { get set } specifier}} // expected-error{{expected get, read, or set in a protocol property}}
  var phone: String { mutate } // expected-error{{property in protocol must have explicit { get } or { get set } specifier}} // expected-error{{expected get, read, or set in a protocol property}}
}

