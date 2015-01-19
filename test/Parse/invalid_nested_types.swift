// RUN: %target-parse-verify-swift

protocol P {
  class GC<X> {} // expected-error{{type not allowed here}}

  protocol P2 { // expected-error{{type not allowed here}}
    var v1: Int { get }
  }

  struct S { // expected-error{{type not allowed here}}
    var v1: Int
  }

  enum E { // expected-error{{type not allowed here}}
  	case C1
  }
}
