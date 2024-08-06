// RUN: %target-typecheck-verify-swift

extension Int {
  func foo() -> Int {}
  var bar: Int {
    get {}
  }

  func baz() -> Int {}
  func baz(_ x: Int = 0) -> Int {}

  func gen<T>() -> T {} // expected-note 2 {{in call to function 'gen()'}} expected-note 2 {{'gen()' declared here}}
}

// https://github.com/swiftlang/swift/issues/74857
func test(i: Int) {
  let _ = i.foo<Int>() // expected-error {{cannot specialize non-generic type '() -> Int'}}

  let _ = i.gen<Int>() // expected-error {{cannot explicitly specialize a generic function}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  let _ = 0.foo<Int>() // expected-error {{cannot specialize non-generic type '() -> Int'}}

  let _ = i.gen<Int> // expected-error {{cannot explicitly specialize a generic function}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  let _ = i.bar<Int> // expected-error {{cannot specialize non-generic type 'Int'}}
  let _ = 0.bar<Int> // expected-error {{cannot specialize non-generic type 'Int'}}
}

extension Bool {
  func foo<T>() -> T {} // expected-note {{'foo()' declared here}}
}

let _: () -> Bool = false.foo<Int> // expected-error {{cannot explicitly specialize a generic function}}

func foo(_ x: Int) {
  _ = {
    _ = x<String> // expected-error {{cannot specialize non-generic type 'Int'}}
  }
}

