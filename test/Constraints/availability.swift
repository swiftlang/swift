// RUN: %target-typecheck-verify-swift -swift-version 4

// Ensure that we do not select the unavailable failable init as the
// only solution and then fail to typecheck.
protocol P {}

class C : P {
  @available(swift, obsoleted: 4)
  public init?(_ c: C) {
  }

  public init<T : P>(_ c: T) {}
}

func f(c: C) {
  let _: C? = C(c)
}

// rdar://problem/60047439 - unable to disambiguite expression based on availablity
func test_contextual_member_with_availability() {
  struct A {
    static var foo: A = A()
  }

  struct B {
    @available(*, unavailable, renamed: "bar")
    static var foo: B = B()
  }

  struct Test {
    init(_: A) {}
    init(_: B) {}
  }

  _ = Test(.foo) // Ok
}

@available(*, unavailable)
func unavailableFunction(_ x: Int) -> Bool { true } // expected-note {{'unavailableFunction' has been explicitly marked unavailable here}}

// SR-13260: Availability checking not working in the where clause of a for
// loop.
func sr13260(_ arr: [Int]) {
  for x in arr where unavailableFunction(x) {} // expected-error {{'unavailableFunction' is unavailable}}
}
