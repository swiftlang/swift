// RUN: %target-typecheck-verify-swift

protocol P {}

func fn<T, U: P>(_ arg1: T, arg2: (T) -> U) {}

func test(str: String) {
  fn(str) { arg in
    <#FOO#> // expected-error {{editor placeholder in source file}}
  }
}
