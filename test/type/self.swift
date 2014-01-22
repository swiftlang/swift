// RUN: %swift -parse %s -verify

struct S0<T> {
  func foo(other: Self) { } // expected-error{{'Self' is only available in a protocol; did you mean 'S0'?}}{{19-23=S0}}
}

class C0<T> {
  func foo(other: Self) { } // expected-error{{'Self' is only available in a protocol; did you mean 'C0'?}}{{19-23=C0}}
}

class E0<T> {
  func foo(other: Self) { } // expected-error{{'Self' is only available in a protocol; did you mean 'E0'?}}{{19-23=E0}}
}
