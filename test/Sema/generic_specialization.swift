// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-

extension Int {
  func foo() -> Int {} // expected-note 3 {{'foo()' declared here}}
  var bar: Int {
    get {}
  }

  func baz() -> Int {}
  func baz(_ x: Int = 0) -> Int {}

  func gen<T>() -> T {} // expected-note 2 {{in call to function 'gen()'}} expected-note 2 {{'gen()' declared here}}
}

// https://github.com/swiftlang/swift/issues/74857
func test(i: Int) {
  let _ = i.foo<Int>()
  // expected-swift5-warning@-1 {{cannot explicitly specialize instance method 'foo()'}}
  // expected-swift6-error@-2 {{cannot explicitly specialize instance method 'foo()'}}

  let _ = i.gen<Int>()
  // expected-swift5-warning@-1 {{cannot explicitly specialize instance method 'gen()'}}
  // expected-swift6-error@-2 {{cannot explicitly specialize instance method 'gen()'}}
  // expected-error@-3 {{generic parameter 'T' could not be inferred}}

  let _ = 0.foo<Int>()
  // expected-swift5-warning@-1 {{cannot explicitly specialize instance method 'foo()'}}
  // expected-swift6-error@-2 {{cannot explicitly specialize instance method 'foo()'}}

  let _ = i.gen<Int>
  // expected-swift5-warning@-1 {{cannot explicitly specialize instance method 'gen()'}}
  // expected-swift6-error@-2 {{cannot explicitly specialize instance method 'gen()'}}
  // expected-error@-3 {{generic parameter 'T' could not be inferred}}
  let _ = i.bar<Int>
  // expected-swift5-error@-1 {{cannot specialize non-generic type 'Int'}}
  // expected-swift6-error@-2 {{cannot specialize non-generic type 'Int'}}
  let _ = 0.bar<Int>
  // expected-swift5-error@-1 {{cannot specialize non-generic type 'Int'}}
  // expected-swift6-error@-2 {{cannot specialize non-generic type 'Int'}}
}

func testOptionalChain(i: Int?) {
  let _ = i?.foo<Int>()
  // expected-swift5-warning@-1 {{cannot explicitly specialize instance method 'foo()'}}
  // expected-swift6-error@-2 {{cannot explicitly specialize instance method 'foo()'}}
}

extension Bool {
  func foo<T>() -> T {} // expected-note {{'foo()' declared here}}
}

let _: () -> Bool = false.foo<Int>
// expected-swift5-warning@-1 {{cannot explicitly specialize instance method 'foo()'}}
// expected-swift6-error@-2 {{cannot explicitly specialize instance method 'foo()'}}

func foo(_ x: Int) {
  _ = {
    _ = x<String>
    // expected-swift5-error@-1 {{cannot specialize non-generic type 'Int'}}
    // expected-swift6-error@-2 {{cannot specialize non-generic type 'Int'}}
  }
}

do {
  struct Test<T> {
    init(_: (T) -> Void) {} // expected-note {{'init(_:)' declared here}}
  }

  _ = Test.init<Int>({ (_: Int) -> Void in })
  // expected-swift5-warning@-1 {{cannot explicitly specialize initializer 'init(_:)'}}
  // expected-swift6-error@-2 {{cannot explicitly specialize initializer 'init(_:)'}}
}

do {
  // expected-error@+1:13 {{cannot specialize non-generic type 'module<Swift>'}}{{none}}
  func f(_: Swift<Int>) {}
}
