// RUN: %target-typecheck-verify-swift

protocol P {
  subscript<Value>(x: Value) -> Int { // expected-note {{protocol requires subscript with type '<Value> (Value) -> Int'}}
    get
  }
}

struct S : P { // expected-error {{type 'S' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  subscript<Value>(x: Int) -> Value { // expected-note {{candidate has non-matching type '<Value> (Int) -> Value'}}
  } // missing return expectations moved to `SILOptimizer/missing_returns`
}

struct S2: P {
  subscript<Value>(x: Value) -> Int {
    return 123
  }
}

protocol P2 {
  subscript(x: Int) -> Int {
    get
  }
}

struct S3: P2 {
  subscript(x: Int) -> Int {
    return x
  }
}
