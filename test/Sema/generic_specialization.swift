// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-

extension Int {
  func foo() -> Int {} // expected-note 3 {{'foo()' declared here}}
  var bar: Int {
    get {}
  }

  func baz() -> Int {}
  func baz(_ x: Int = 0) -> Int {}

  func gen<T>(_ x: T) -> T {}
}

// https://github.com/swiftlang/swift/issues/74857
func test(i: Int) {
  let _ = i.foo<Int>()
  // expected-swift5-warning@-1 {{cannot explicitly specialize instance method 'foo()'}}
  // expected-swift6-error@-2 {{cannot explicitly specialize instance method 'foo()'}}

  let _ = i.gen<Int>(0) // ok, explicit specialization is allowed

  let _ = 0.foo<Int>()
  // expected-swift5-warning@-1 {{cannot explicitly specialize instance method 'foo()'}}
  // expected-swift6-error@-2 {{cannot explicitly specialize instance method 'foo()'}}

  let genInt = i.gen<Int> // ok, forms function reference to explicitly specialized method
  let _ = genInt(0)

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
  func foo<T>() -> T {}
}

let _: () -> Bool = false.foo<Int>
// expected-error@-1 {{type of expression is ambiguous}}
let _: () -> Bool = false.foo

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
