// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: swift_feature_CoroutineAccessors

public class Klass {
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

protocol P {
  var name: String { borrow }
  var phone: String { borrow mutate }
  var address: String { mutate } // expected-error{{variable with a 'mutate' accessor must also have a 'borrow' accessor}}
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

public protocol Q {
  var id: NonTrivial { borrow mutate }
}

public struct NonTrivial {
  public var k: Klass
}

// Protocol requirements witnessed via borrow/mutate accessors
struct S1 : Q {
  var _id: NonTrivial

  var id: NonTrivial {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

// Protocol requirements witnessed via stored property
struct S2 : Q {
  var id: NonTrivial
}
 
public struct S3 : Q {
  public var _id: NonTrivial

  public var id: NonTrivial { // expected-error {{borrow/mutate requirement cannot be satisfied by 'id'}}
    _read {
      yield _id
    }
    _modify {
      yield &_id
    }
  }
}

struct S4 : Q {
  var _id: NonTrivial

  var id: NonTrivial { // expected-error {{borrow/mutate requirement cannot be satisfied by 'id'}}
    get {
      return _id
    }
    set {
      _id = newValue
    }
  }
}

struct S5 : Q {
  var _id: NonTrivial
}

extension S5 {
  var id: NonTrivial {
    borrow  {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

struct S6 : Q { // expected-error {{borrow/mutate requirement cannot be satisfied by 'id'}}
  var _id: NonTrivial
}

extension S6 {
  var id: NonTrivial {
    get {
      return _id
    }
    set {
      _id = newValue
    }
  }
}

