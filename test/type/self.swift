// RUN: %target-typecheck-verify-swift

struct S0<T> {
  func foo(_ other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'S0'?}}{{21-25=S0}}
}

class C0<T> {
  func foo(_ other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'C0'?}}{{21-25=C0}}
}

enum E0<T> {
  func foo(_ other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'E0'?}}{{21-25=E0}}
}

// rdar://problem/21745221
struct X {
  typealias T = Int
}

extension X {
  struct Inner {
  }
}

extension X.Inner {
  func foo(_ other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'Inner'?}}{{21-25=Inner}}
}
