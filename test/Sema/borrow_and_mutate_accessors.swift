// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: swift_feature_CoroutineAccessors

class Klass {
  var _i: Int = 0
}

struct Struct {
  var _i: Int = 0
  var _k: Klass

  var k1: Klass {
    consuming borrow { // expected-error{{a 'borrow' accessor cannot be declared consuming}}
      return _k
    }
    consuming mutate { // expected-error{{a 'mutate' accessor cannot be declared consuming}}
      return &_k
    }
  }

  var k2: Klass {
    mutating borrow { // expected-error{{mutating ownership modifier is not yet supported on a 'borrow' accessor}}
      return _k
    }
    nonmutating mutate { // expected-error{{nonmutating ownership modifier is not yet supported on a 'mutate' accessor}}
      return &_k // expected-error{{'&' may only be used to pass an argument to inout parameter}}
    }
  }

  var k3: Klass {
    nonmutating borrow {
      return _k
    }
    mutating mutate {
      return &_k
    }
  }
}

extension Klass {
  var i: Int {
    borrow { // expected-error{{a 'borrow' accessor is supported only on a struct}}
      return 0
    }
    mutate { // expected-error{{a 'mutate' accessor is supported only on a struct}}
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

enum OrderStatus: ~Copyable {
  case processing(trackingNumber: String)
  case cancelled(reason: String)
  
  var description: String {
    borrow { // expected-error{{a 'borrow' accessor is supported only on a struct}}
      switch self {
        case .processing(let trackingNumber):
          return trackingNumber
        case .cancelled(let reason):
          return reason
      }
    }
  }
}

