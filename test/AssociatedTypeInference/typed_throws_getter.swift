// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype E: Error
  var prop: Int { get throws(E) }  // expected-note {{protocol requires property 'prop' with type 'Int'}}
}

struct S0: P {
  var prop: Int {
    get {}
  }
}

struct S1: P {
  var prop: Int {
    get throws(Never) {}
  }
}

struct S2: P {
  var prop: Int {
    get throws {}
  }
}

struct S3: P {
  var prop: Int {
    get throws(any Error) {}
  }
}

struct MyError: Error {}

struct S4: P {
  var prop: Int {
    get throws(MyError) {}
  }
}

struct S5<E: Error>: P {
  var prop: Int {
    get throws(E) {}
  }
}

// Invalid example

struct S6: P { // expected-error {{type 'S6' does not conform to protocol 'P'}}
  // expected-note@-1 {{add stubs for conformance}}

  typealias E = MyError
  var prop: Int {  // expected-note {{candidate has non-matching type 'Int'}}
    get throws(any Error) {
      fatalError()
    }
  }
}
