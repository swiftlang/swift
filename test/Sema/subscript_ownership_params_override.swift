// RUN: %target-typecheck-verify-swift

struct NC: ~Copyable { var v: Int }

// MARK: overriding

class Base {
  func fnDefault(_ x: String) {}
  func fnBorrowing(_ x: borrowing String) {}
  func fnInout(_ x: inout String) {}
  // expected-note@-1 {{potential overridden instance method 'fnInout' here}}

  subscript(dflt x: String) -> Int { return 0 }
  subscript(borrow x: borrowing String) -> Int { return 0 }
  // expected-note@-1 {{potential overridden subscript 'subscript(borrow:)' here}}
  subscript(io x: inout String) -> Int {
    // expected-note@-1 {{potential overridden subscript 'subscript(io:)' here}}
    get { return 0 }
    set { }
  }
  subscript(nc x: borrowing NC) -> Int { return x.v }
  // expected-note@-1 {{potential overridden subscript 'subscript(nc:)' here}}
  subscript(ncio x: inout NC) -> Int {
    get { return x.v }
    set { }
  }
}

// `borrowing` and the default are interchangeable, for both.
class DerivedOK: Base {
  override func fnDefault(_ x: borrowing String) {}
  override func fnBorrowing(_ x: String) {}
  override subscript(dflt x: borrowing String) -> Int { return 1 }
  override subscript(borrow x: String) -> Int { return 1 }
}

// Matching `inout` is an override, including for a noncopyable index.
class DerivedInOutOK: Base {
  override func fnInout(_ x: inout String) {}
  override subscript(io x: inout String) -> Int {
    get { return 1 }
    set { }
  }
  override subscript(ncio x: inout NC) -> Int {
    get { return x.v + 1 }
    set { }
  }
}

// Changing `inout` changes the type, so it is not an override.
class DerivedInOutBad: Base {
  override func fnInout(_ x: borrowing String) {}
  // expected-error@-1 {{method does not override any method from its superclass}}
  override subscript(io x: borrowing String) -> Int { return 1 }
  // expected-error@-1 {{subscript does not override any subscript from its superclass}}
  override subscript(borrow x: inout String) -> Int {
    // expected-error@-1 {{subscript does not override any subscript from its superclass}}
    get { return 1 }
    set { }
  }
  override subscript(nc x: inout NC) -> Int {
    // expected-error@-1 {{subscript does not override any subscript from its superclass}}
    get { return x.v }
    set { }
  }
}

// MARK: witnessing

protocol P {
  func fnBorrowing(_ x: borrowing String)
  func fnInout(_ x: inout String)
  // expected-note@-1 {{protocol requires function 'fnInout' with type '(inout String) -> ()'}}

  subscript(borrow x: borrowing String) -> Int { get }
  subscript(io x: inout String) -> Int { get set }
  // expected-note@-1 {{protocol requires subscript with type '(inout String) -> Int'}}
  subscript(ncio x: inout NC) -> Int { get set }
}

// `borrowing` vs. the default witnesses fine, and matching `inout` witnesses.
struct WitnessOK: P {
  func fnBorrowing(_ x: String) {}
  func fnInout(_ x: inout String) {}

  subscript(borrow x: String) -> Int { return 0 }
  subscript(io x: inout String) -> Int {
    get { return 0 }
    set { }
  }
  subscript(ncio x: inout NC) -> Int {
    get { return x.v }
    set { }
  }
}

// Dropping `inout` from a requirement is not a witness.
struct WitnessBad: P {
  // expected-error@-1 {{type 'WitnessBad' does not conform to protocol 'P'}}
  // expected-note@-2 {{add stubs for conformance}}
  func fnBorrowing(_ x: borrowing String) {}
  func fnInout(_ x: borrowing String) {}
  // expected-note@-1 {{candidate has non-matching type '(borrowing String) -> ()'}}

  subscript(borrow x: borrowing String) -> Int { return 0 }
  subscript(io x: borrowing String) -> Int {
    // expected-note@-1 {{candidate has non-matching type '(borrowing String) -> Int'}}
    get { return 0 }
    set { }
  }
  subscript(ncio x: inout NC) -> Int {
    get { return x.v }
    set { }
  }
}
