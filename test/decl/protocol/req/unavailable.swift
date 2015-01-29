// RUN: %target-parse-verify-swift

// An @objc protocol can have 'unavailable'
// methods.  They are treated as if they
// were marked optional
@objc protocol Proto {
  @availability(*,unavailable) optional func bad()
  func good()
}

class Foo : Proto {
  @objc func good() {}
}

// Reject protocols with 'unavailable' requirements
// if a protocol is not marked @objc.
protocol NonObjCProto {
  @availability(*,unavailable) func bad() // expected-error {{protocol members can only be marked unavailable in an @objc protocol}} expected-note {{protocol requires function 'bad()'}}
  func good()
}

class Bar : NonObjCProto { // expected-error {{type 'Bar' does not conform to protocol 'NonObjCProto'}}
  func good() {}
}

