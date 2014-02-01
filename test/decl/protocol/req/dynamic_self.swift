// RUN: %swift -parse %s -verify
protocol P {
  func f() -> DynamicSelf // expected-note 3{{protocol requires function 'f' with type '() -> DynamicSelf'}}
}

// Okay: DynamicSelf method in class.
class X : P {
  func f() -> DynamicSelf { return self }
}

class Y {
  func f() -> DynamicSelf { return self }
}

// Okay: DynamicSelf method in superclass.
class Z : Y, P { }

// Error: Z is not DynamicSelf, so subclass would not conform if Z did.
class ZError : P { // expected-error{{type 'ZError' does not conform to protocol 'P'}}
  func f() -> ZError { return self } // expected-error{{candidate result type is not 'DynamicSelf'}}
}

// Okay: struct conforms by returning itself
struct S : P {
  func f() -> S { return self }
}

struct SError : P { // expected-error{{type 'SError' does not conform to protocol 'P'}}
  func f() -> Int { return 0 } // expected-note{{candidate has non-matching type '() -> Int'}}
}

// Okay: enum conforms by returning itself
enum E : P {
  func f() -> E { return self }
}

enum EError : P { // expected-error{{type 'EError' does not conform to protocol 'P'}}
  func f() -> Int { return 0 } // expected-note{{candidate has non-matching type '() -> Int'}}
}
