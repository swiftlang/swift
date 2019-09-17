// RUN: %target-typecheck-verify-swift

protocol P {}

func fn<T, U: P>(_ arg1: T, arg2: (T) -> U) {}
// expected-note@-1 {{required by global function 'fn(_:arg2:)' where 'U' = '()'}}

func test(str: String) {
  fn(str) { arg in // expected-error {{type '()' cannot conform to 'P'; only struct/enum/class types can conform to protocols}}
    <#FOO#> // expected-error {{editor placeholder in source file}}
  }
}
