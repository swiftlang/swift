// RUN: %target-swift-frontend -enable-experimental-feature Lifetimes -emit-sil -verify %s
// REQUIRES: swift_feature_Lifetimes

struct A: ~Copyable {
  var gotten: B {
    get { fatalError() }
  }
  
  let _borrowed: B 
  var borrowed: B {
    borrow { return _borrowed }
  }
  
  var forwarded: B {
    consuming get { fatalError() }
  }
}

struct B: ~Copyable {
  var forwarded: C {
    consuming get { fatalError() }
  }
}

struct C: ~Copyable {}

func consume(_: consuming C) {}

func exerciseBorrow(a: borrowing A) { // expected-error{{'a' is borrowed and cannot be consumed}}
  consume(a.gotten.forwarded)
  consume(a.borrowed.forwarded) // expected-error{{'a.borrowed' is borrowed and cannot be consumed}} expected-note{{consumed here}}
  consume(a.forwarded.forwarded) // expected-note{{consumed here}}
  consume(a.forwarded.forwarded) // expected-note{{consumed here}}
}

func exerciseConsume(a: consuming A) { // expected-error{{'a' used after consume}}
  consume(a.gotten.forwarded)
  consume(a.borrowed.forwarded) // expected-error{{'a.borrowed' is borrowed and cannot be consumed}} expected-note{{consumed here}}
  consume(a.forwarded.forwarded) // expected-note{{consumed here}}
  
  // `a.forwarded` consumed `a`, so these should be errors:
  consume(a.gotten.forwarded) // expected-note{{used here}}
  consume(a.forwarded.forwarded)
}

struct AA: ~Copyable {
  var x: Any

  var gotten: AB {
    get { fatalError() }
  }
  
  let _borrowed: AB 
  var borrowed: AB {
    borrow { return _borrowed }
  }
  
  var forwarded: AB {
    consuming get { fatalError() }
  }
}

struct AB: ~Copyable {
  var x: Any
  
  var forwarded: AC {
    consuming get { fatalError() }
  }
}

struct AC: ~Copyable { var x: Any }

func consume(_: consuming AC) {}

func exerciseBorrowAddr(a: borrowing AA) { // expected-error 2 {{'a' is borrowed and cannot be consumed}}
  consume(a.gotten.forwarded)
  consume(a.borrowed.forwarded) // expected-error{{'a.borrowed' is borrowed and cannot be consumed}} expected-note{{consumed here}}
  consume(a.forwarded.forwarded) // expected-note{{consumed here}}
  consume(a.forwarded.forwarded) // expected-note{{consumed here}}
}

func exerciseConsumeAddr(a: consuming AA) { // expected-error{{'a' used after consume}}
  consume(a.gotten.forwarded)
  consume(a.borrowed.forwarded) // expected-error{{'a.borrowed' is borrowed and cannot be consumed}} expected-note{{consumed here}}
  consume(a.forwarded.forwarded) // expected-note{{consumed here}}
  
  // `a.forwarded` consumed `a`, so these should be errors:
  consume(a.gotten.forwarded) // expected-note{{used here}}
  consume(a.forwarded.forwarded)
}

// rdar://189942810

extension Optional where Wrapped: ~Copyable, Wrapped: ~Escapable {
  var unsightlyUnwrapped: Wrapped {
    @_lifetime(copy self)
    consuming get { fatalError() }
  }
}

extension Optional where Wrapped: ~Copyable, Wrapped: Escapable {
  var mewtableRef: MutableRef<Wrapped>? {
    @_lifetime(&self)
    mutating get { fatalError() }
  }
  
  @_lifetime(&self)
  public mutating func putt(_ new: consuming Wrapped) -> MutableRef<Wrapped> {
    self = .some(new)
    return mewtableRef.unsightlyUnwrapped
  }
}
