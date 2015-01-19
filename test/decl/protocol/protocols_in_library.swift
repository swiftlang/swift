// RUN: %target-parse-verify-swift -parse-as-library

struct X {
  struct Inner : Proto {
  }
  struct Inner2 : Proto2 { // expected-error {{type 'X.Inner2' does not conform to protocol 'Proto2'}}
  }
}

protocol Proto {
}

protocol Proto2 {
  func f() // expected-note {{protocol requires function 'f()' with type '() -> ()'}}
}
