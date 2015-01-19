// RUN: %target-parse-verify-swift

protocol P {
  func f() -> Self // expected-note 2{{protocol requires function 'f()' with type '() -> Self'}}
}

// Okay: Self method in class.
class X : P {
  func f() -> Self { return self }
}

class Y {
  func f() -> Self { return self }
}

class GX<T> : P {
  func f() -> Self { return self }
}

// Okay: dynamic Self method in superclass.
class Z : Y, P { }

// Erro: Z2 conforms, but subclass would not
class Z2 : P {
  func f() -> Z2 { return self } // expected-error{{method 'f()' in non-final class 'Z2' must return `Self` to conform to protocol 'P'}}
}

// Okay: struct conforms by returning itself
struct S : P {
  func f() -> S { return self }
}

struct GS<T> : P {
  func f() -> GS { return self }
}

struct SError : P { // expected-error{{type 'SError' does not conform to protocol 'P'}}
  func f() -> Int { return 0 } // expected-note{{candidate has non-matching type '() -> Int'}}
}

// Okay: enum conforms by returning itself
enum E : P {
  func f() -> E { return self }
}

enum GE<T> : P {
  func f() -> GE { return self }
}

enum EError : P { // expected-error{{type 'EError' does not conform to protocol 'P'}}
  func f() -> Int { return 0 } // expected-note{{candidate has non-matching type '() -> Int'}}
}
