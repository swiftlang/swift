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

// rdar://problem/60047439 - unable to disambiguate expression based on availability
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

/// https://github.com/apple/swift/issues/55700
/// Availability checking not working in the `where` clause of a `for` loop
func f_55700(_ arr: [Int]) {
  for x in arr where unavailableFunction(x) {} // expected-error {{'unavailableFunction' is unavailable}}
}

// rdar://101814209
public struct Box<T> {
  @usableFromInline
  let value: T
}

@available(macOS, unavailable)
extension Box where T == Int {
  @usableFromInline
  init(value: T) {
    self.value = value
  }
}

@available(macOS, unavailable)
@_alwaysEmitIntoClient public func aeicFunction() {
  // Select the unavailable @usableFromInline init over the synthesized internal
  // memberwise initializer.
  _ = Box(value: 42)
}

// rdar://87403752 - ambiguity with member declared in unavailable extension
struct HasUnavailableExtesion {
}

@available(*, unavailable)
extension HasUnavailableExtesion {
  static var foo: Self = HasUnavailableExtesion()
}

func test_contextual_member_with_unavailable_extension() {
  struct A {
    static var foo: A = A()
  }

  struct Test {
    init(_: A) {}
    init(_: HasUnavailableExtesion) {}
  }

  _ = Test(.foo) // Ok `A.foo` since `foo` from `HasUnavailableExtesion` is unavailable
}
