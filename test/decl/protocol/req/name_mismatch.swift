// RUN: %swift -parse %s -verify

protocol P {
  func foo(i: Int, x: Float) // expected-note 4{{requirement 'foo(i:x:)' declared here}}
}

struct S1 : P {
  func foo(i: Int, x: Float) { }
}

struct S2 : P {
  func foo(i: Int, _ x: Float) { } // expected-error{{method 'foo(i:_:)' has different argument names than those required by protocol 'P' ('foo(i:x:)')}}{{20-22=}}
}

struct S3 : P {
  func foo(i: Int, y: Float) { } // expected-error{{method 'foo(i:y:)' has different argument names than those required by protocol 'P' ('foo(i:x:)')}}{{20-20=x }}
}

struct S4 : P {
  func foo(i: Int, Float) { } // expected-error{{method 'foo(i:_:)' has different argument names than those required by protocol 'P' ('foo(i:x:)')}}{{20-20=x: }}
}

struct S5 : P {
  func foo(i: Int, z x: Float) { } // expected-error{{method 'foo(i:z:)' has different argument names than those required by protocol 'P' ('foo(i:x:)')}}{{20-22=}}
}
