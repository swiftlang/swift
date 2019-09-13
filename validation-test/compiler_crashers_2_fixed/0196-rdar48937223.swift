// RUN: %target-typecheck-verify-swift

protocol P {}

func fn<T, U: P>(_ arg1: T, arg2: (T) -> U) {} // expected-note {{where 'U' = '()'}}

func test(str: String) {
  fn(str) { arg in // expected-error {{global function 'fn(_:arg2:)' requires that '()' conform to 'P'}}
    <#FOO#> // expected-error {{editor placeholder in source file}}
  }
}
