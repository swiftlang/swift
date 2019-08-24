// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx

// Make sure that a compatible unavailable wrapping doesn't allow referencing declarations that are completely unavailable.

@available(iOS, unavailable)
class Outer {
  @available(*, unavailable)
  func completelyBadMethod() {} // expected-note {{'completelyBadMethod()' has been explicitly marked unavailable here}}
}

@available(iOS, unavailable)
func test(outer: Outer) {
  outer.completelyBadMethod() // expected-error {{'completelyBadMethod()' is unavailable}}
}

@available(*, unavailable)
class Outer2 { // expected-note {{'Outer2' has been explicitly marked unavailable here}}
	@available(iOS, unavailable)
    func innerUnavailable() {}
}
@available(iOS, unavailable)
func test2(outer: Outer2) { // expected-error {{'Outer2' is unavailable}}
  outer.innerUnavailable()
}
