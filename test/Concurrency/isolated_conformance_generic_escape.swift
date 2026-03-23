// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -swift-version 6 -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -swift-version 5 -strict-concurrency=complete -verify-additional-prefix swift5-

// REQUIRES: concurrency

// This verifies additional enforcement of Rule (1) from the
// isolated conformances proposal: https://github.com/swiftlang/swift-evolution/blob/main/proposals/0470-isolated-conformances.md
// We must not allow escaping an isolated conformance through generic parameters
// into a context that is isolated differently, because we may then end up calling e.g.
// a synchronous requirement from the wrong isolated context and violate actor isolation.
// rdar://173120506

import _Concurrency

protocol MyProtocol {
  func doSomething()
}

@MainActor
class MySyncClass: @MainActor MyProtocol {
  func doSomething() {
    MainActor.shared.assertIsolated()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: @concurrent callees — disallowed

@concurrent
func callDoSomething(_ p: some MyProtocol) async {
  p.doSomething() // bad, would call actor synchronously from other context
}

// ==== -----------------------------------------------------------------------
// MARK: nonisolated(nonsending) callees — inherits caller's executor but could forward and break isolation

nonisolated(nonsending) func callDoSomethingNonisolatedNonsending(_ p: some MyProtocol) async {
  p.doSomething() // may or may not be bad, depending who the caller was
}

@concurrent func whoopsThisWouldHaveBeenBad(_ p: some MyProtocol) async {
  p.doSomething()
}

// ==== -----------------------------------------------------------------------
// MARK: Different global actor callee - disallowed

@globalActor actor OtherActor {
  static let shared = OtherActor()
}

@OtherActor
func callDoSomethingFromOtherActor(_ p: some MyProtocol) async {
  p.doSomething()
}

// ==== -----------------------------------------------------------------------
// MARK: @MainActor callee — same isolation domain, ok

@MainActor
func callDoSomethingFromMainActor(_ p: some MyProtocol) async {
  p.doSomething()
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

@available(SwiftStdlib 6.2, *)
@MainActor
func test() async {
  // @concurrent async - don't allow changing execution context, would allow synchronous call on actor
  // expected-swift6-error@+2{{main actor-isolated conformance of 'MySyncClass' to 'MyProtocol' cannot be used in nonisolated context}}
  // expected-swift5-warning@+1{{main actor-isolated conformance of 'MySyncClass' to 'MyProtocol' cannot be used in nonisolated context; this is an error in the Swift 6 language mode}}
  await callDoSomething(MySyncClass())

  // nonisolated(nonsending) - is not allowed either, it could escape the value to another @concurrent method and break isolation there
  // expected-swift6-error@+2{{main actor-isolated conformance of 'MySyncClass' to 'MyProtocol' cannot be used in caller isolation inheriting-isolated context}}
  // expected-swift5-warning@+1{{main actor-isolated conformance of 'MySyncClass' to 'MyProtocol' cannot be used in caller isolation inheriting-isolated context; this is an error in the Swift 6 language mode}}
  await callDoSomethingNonisolatedNonsending(MySyncClass())

  // Different global actor callee — not allowed, same as hopping to global executor, we don't allow ANY other isolation
  // expected-swift6-error@+2{{main actor-isolated conformance of 'MySyncClass' to 'MyProtocol' cannot be used in global actor 'OtherActor'-isolated context}}
  // expected-swift5-warning@+1{{main actor-isolated conformance of 'MySyncClass' to 'MyProtocol' cannot be used in global actor 'OtherActor'-isolated context; this is an error in the Swift 6 language mode}}
  await callDoSomethingFromOtherActor(MySyncClass())

  // @MainActor callee — same caller isolation, this is allowed
  await callDoSomethingFromMainActor(MySyncClass())
}
