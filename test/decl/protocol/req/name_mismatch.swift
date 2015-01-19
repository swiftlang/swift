// RUN: %target-parse-verify-swift

protocol P {
  func foo(i: Int, x: Float) // expected-note 4{{requirement 'foo(_:x:)' declared here}}
}

struct S1 : P {
  func foo(i: Int, x: Float) { }
}

struct S2 : P {
  func foo(i: Int, _ x: Float) { } // expected-error{{method 'foo' has different argument names from those required by protocol 'P' ('foo(_:x:)')}}{{20-22=}}
}

struct S3 : P {
  func foo(i: Int, y: Float) { } // expected-error{{method 'foo(_:y:)' has different argument names from those required by protocol 'P' ('foo(_:x:)')}}{{20-20=x }}
}

struct S4 : P {
  func foo(i: Int, Float) { } // expected-error{{method 'foo' has different argument names from those required by protocol 'P' ('foo(_:x:)')}}{{20-20=x: }}
}

struct S5 : P {
  func foo(i: Int, z x: Float) { } // expected-error{{method 'foo(_:z:)' has different argument names from those required by protocol 'P' ('foo(_:x:)')}}{{20-22=}}
}

struct Loadable { }

protocol LabeledRequirement {
  func method(#x: Loadable)
}

struct UnlabeledWitness : LabeledRequirement {
  func method(x _: Loadable) {}
}
