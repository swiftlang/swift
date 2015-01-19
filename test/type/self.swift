// RUN: %target-parse-verify-swift

struct S0<T> {
  func foo(other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a class method; did you mean 'S0'?}}{{19-23=S0}}
}

class C0<T> {
  func foo(other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a class method; did you mean 'C0'?}}{{19-23=C0}}
}

enum E0<T> {
  func foo(other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a class method; did you mean 'E0'?}}{{19-23=E0}}
}
