// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -swift-version 6

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

  @MainActor
  func doSomethingOnMainActor()
}

@MainActor
class MyClass: @MainActor MyProtocol {
  func doSomething() {
    MainActor.shared.assertIsolated()
  }

  @MainActor
  func doSomethingOnMainActor() {
    MainActor.shared.assertIsolated()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: @concurrent callees — disallowed

@concurrent
func callDoSomethingConcurrent(_ p: some MyProtocol) async {
  p.doSomething() // bad, would call actor synchronously from other context
}

// ==== -----------------------------------------------------------------------
// MARK: nonisolated(nonsending) callees — inherits caller's executor

nonisolated(nonsending) func callDoSomethingNonisolatedNonsending(_ p: some MyProtocol) async {
  p.doSomething() // ok, inherits caller's isolation
}

// ==== -----------------------------------------------------------------------
// MARK: nonisolated(nonsending) that forwards to @concurrent — error in body

// When the conformance is concrete (not generic), the error is caught at the
// forwarding call site inside the nonisolated(nonsending) function body.
nonisolated(nonsending) func nonsendingForwardToConcurrent(_ p: MyClass) async {
  // expected-error@+1{{main actor-isolated conformance of 'MyClass' to 'MyProtocol' cannot be used in @concurrent context}}
  await callDoSomethingConcurrent(p)
}

// When the conformance is abstract (generic), we reject it because the concrete
// conformance may be isolated.
nonisolated(nonsending) func nonsendingForwardToConcurrentGeneric(_ p: some MyProtocol) async {
  // expected-warning@+1{{conformance of underlying type of 'some MyProtocol' to protocol 'MyProtocol' may be isolated and cannot be passed to @concurrent context}}
  await callDoSomethingConcurrent(p)
}

struct Container<T: MyProtocol> {
  nonisolated(nonsending) func nonsendingForwardToConcurrentGeneric(_ p: T) async {
    // expected-warning@+1{{conformance of 'T' to protocol 'MyProtocol' may be isolated and cannot be passed to @concurrent context}}
    await callDoSomethingConcurrent(p)
  }
}

// Sendable protocols cannot have isolated conformances, so forwarding is safe.
protocol SendableProtocol: Sendable {
  func doSomething()
}

@concurrent
func callSendableProtocol(_ p: some SendableProtocol) async {
  p.doSomething()
}

@concurrent
func callSendableProtocolComp(_ p: some (MyProtocol & Sendable)) async {
  p.doSomething()
}

nonisolated(nonsending) func nonsendingForwardSendableGeneric(_ p: some SendableProtocol) async {
  await callSendableProtocol(p) // ok, Sendable protocols can't have isolated conformances
}

nonisolated(nonsending) func nonsendingForwardSendableGenericComp<P: MyProtocol>(_ p: P) async { // expected-note{{consider making generic parameter 'P' conform to the 'Sendable' protocol}}
  // expected-warning@+2{{conformance of 'P' to protocol 'MyProtocol' may be isolated and cannot be passed to @concurrent context}}
  // expected-error@+1{{type 'P' does not conform to the 'Sendable' protocol}}
  await callSendableProtocolComp(p)
}

final class Caller {
  @concurrent func call(first p: some MyProtocol, second fine: String) async { p.doSomething() }
}
nonisolated(nonsending) func nonsendingForwardSendableGeneric(caller: Caller, _ p: some MyProtocol) async {
  // expected-warning@+1{{conformance of underlying type of 'some MyProtocol' to protocol 'MyProtocol' may be isolated and cannot be passed to @concurrent context}}
  await caller.call(first: p, second: "this is fine")
}

// Sendable superclass — a generic parameter constrained to a Sendable
// superclass is itself Sendable, so its conformances cannot be isolated.
class SendableBase: @unchecked Sendable {}
class SendableMiddle: SendableBase, @unchecked Sendable {}

@concurrent
func callSendableSuperclass<T: SendableMiddle & MyProtocol>(_ p: T) async {
  p.doSomething()
}

nonisolated(nonsending) func nonsendingForwardSendableSuperclass<T: SendableMiddle & MyProtocol>(_ p: T) async {
  await callSendableSuperclass(p) // ok, Sendable superclass means no isolated conformances
}

// Callee with explicit generic parameter — the name appears in the diagnostic.
@concurrent
func callDoSomethingConcurrentExplicit<ItsMe: MyProtocol>(_ p: ItsMe) async {
  p.doSomething()
}

nonisolated(nonsending) func nonsendingForwardExplicitGeneric<TheProblemIsNotHere: MyProtocol>(_ p: TheProblemIsNotHere) async {
  // expected-warning@+1{{conformance of 'TheProblemIsNotHere' to protocol 'MyProtocol' may be isolated and cannot be passed to @concurrent context}}
  await callDoSomethingConcurrentExplicit(p)
}

@concurrent func nonsendingForwardSendableGeneric(_ p: some MyProtocol) async {
  await p.doSomethingOnMainActor() // ok: requirement is @MainActor, hop is explicit
}

// @MainActor forwarding generic to @concurrent — same issue, rejected.
@MainActor
func mainActorForwardToConcurrentGeneric(_ p: some MyProtocol) async {
  // expected-warning@+1{{conformance of underlying type of 'some MyProtocol' to protocol 'MyProtocol' may be isolated and cannot be passed to @concurrent context}}
  await callDoSomethingConcurrent(p)
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

@MainActor
func test1() async {
  // Different global actor callee — not allowed, same as hopping to global executor, we don't allow ANY other isolation
  // expected-error@+1{{main actor-isolated conformance of 'MyClass' to 'MyProtocol' cannot be used in global actor 'OtherActor'-isolated context}}
  await callDoSomethingFromOtherActor(MyClass())
}

// ==== -----------------------------------------------------------------------
// MARK: @MainActor callee — same isolation domain, ok

@MainActor
func callDoSomethingFromMainActor(_ p: some MyProtocol) async {
  p.doSomething()
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

@MainActor
func test() async {
  // @concurrent async - don't allow changing execution context, would allow synchronous call on actor
  // expected-error@+1{{main actor-isolated conformance of 'MyClass' to 'MyProtocol' cannot be used in @concurrent context}}
  await callDoSomethingConcurrent(MyClass())

  // nonisolated(nonsending) — inherits caller's isolation, this is allowed
  await callDoSomethingNonisolatedNonsending(MyClass())

  // nonisolated(nonsending) that forwards to @concurrent — the error is
  // caught at the forwarding call site inside the func, not here
  await nonsendingForwardToConcurrent(MyClass())
  await nonsendingForwardToConcurrentGeneric(MyClass())

  // @MainActor callee — same caller isolation, this is allowed
  await callDoSomethingFromMainActor(MyClass())
}
