// RUN: %target-typecheck-verify-swift -parse-stdlib -target arm64-apple-macos11

// expected-warning@<unknown> * {{using sysroot for }}

// Make sure that a compatible unavailable wrapping doesn't allow referencing declarations that are completely unavailable.

@available(iOS, unavailable)
class Outer {
  @available(*, unavailable) // expected-note {{'completelyBadMethod()' has been explicitly marked unavailable here}}
  func completelyBadMethod() {}
}

@available(iOS, unavailable)
func test(outer: Outer) {
  outer.completelyBadMethod() // expected-error {{'completelyBadMethod()' is unavailable}}
}

@available(*, unavailable) // expected-note {{'Outer2' has been explicitly marked unavailable here}}
class Outer2 {
	@available(iOS, unavailable)
    func innerUnavailable() {}
}
@available(iOS, unavailable)
func test2(outer: Outer2) { // expected-error {{'Outer2' is unavailable}}
  outer.innerUnavailable()
}
