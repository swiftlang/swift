// RUN: %target-typecheck-verify-swift

protocol Proto {
  init?(string: String)
  func foo()
}

struct TypedFoo<T: Hashable, Proto> {
  // expected-note@-1 {{did you mean to declare 'Proto' as a protocol conformance for 'T'?}} {{28-29= &}}
  func foo(value: String) {
    if let str = T.init(string:value) { // expected-error {{type 'T' has no member 'init'}}
      print(str)
    }
  }
}

func bar<T, Proto>(arg: T) { // expected-error {{generic parameter 'Proto' is not used in function signature}}
  // expected-note@-1 {{did you mean to declare 'Proto' as a protocol conformance for 'T'?}} {{11-12=:}}
  arg.foo() // expected-error {{value of type 'T' has no member 'foo'}}
}

func baz<T: Hashable, Proto>(arg: T) { // expected-error {{generic parameter 'Proto' is not used in function signature}}
  arg.ugh() // expected-error {{value of type 'T' has no member 'ugh'}}
}
