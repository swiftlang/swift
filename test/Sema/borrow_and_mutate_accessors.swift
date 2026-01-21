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
  var id: NonTrivial { borrow mutate } // expected-note{{protocol requires property 'id' with type 'NonTrivial'}} // expected-note{{}} // expected-note{{}} // expected-note{{}}
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
 
public struct S3 : Q { // expected-error{{type 'S3' does not conform to protocol 'Q'}} // expected-note{{add stubs for conformance}}
  public var _id: NonTrivial

  public var id: NonTrivial { // expected-error {{candidate is not a stored property or a computed property with borrow/mutate accessor, but protocol requires it}}
    _read {
      yield _id
    }
    _modify {
      yield &_id
    }
  }
}

struct S4 : Q { // expected-error{{type 'S4' does not conform to protocol 'Q'}} // expected-note{{add stubs for conformance}}

  var _id: NonTrivial

  var id: NonTrivial { // expected-error {{candidate is not a stored property or a computed property with borrow/mutate accessor, but protocol requires it}}
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

struct S6 : Q { // expected-error{{type 'S6' does not conform to protocol 'Q'}} // expected-note{{add stubs for conformance}}
  var _id: NonTrivial
}

extension S6 {
  var id: NonTrivial { // expected-error{{candidate is not a stored property or a computed property with borrow/mutate accessor, but protocol requires it}}
    get {
      return _id
    }
    set {
      _id = newValue
    }
  }
}

public struct S7 : Q { // expected-error{{type 'S7' does not conform to protocol 'Q'}} // expected-note{{add stubs for conformance}}
  public var _id: NonTrivial

  public var id: NonTrivial { // expected-error {{candidate is not a stored property or a computed property with borrow/mutate accessor, but protocol requires it}}
    yielding borrow {
      yield _id
    }
    yielding mutate {
      yield &_id
    }
  }
}

// Borrow and mutate accessors with property observers
struct S8 {
  var _i: Int

  var i: Int {
    borrow {
      return _i
    }
    mutate {
      return &_i
    }
    willSet {} // expected-error {{'willSet' cannot be provided together with a 'borrow' accessor}}
    didSet {} // expected-error {{'didSet' cannot be provided together with a 'borrow' accessor}}
  }
}
