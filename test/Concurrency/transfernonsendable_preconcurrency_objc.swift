// RUN: %target-swift-frontend -emit-sil -swift-version 6 -verify %s -o /dev/null -import-objc-header %S/Inputs/transfernonsendable_preconcurrency_objc.h

// REQUIRES: objc_interop
// REQUIRES: concurrency

// Pattern: a @MainActor Swift class overrides a non-isolated ObjC method
// and then calls another @MainActor method on `self`. Both sides of the
// resulting `ApplyExpr` isolation crossing carry the `preconcurrency`
// flag (the override inherits the ObjC method's lack of isolation under
// preconcurrency rules), so the AST-level ActorIsolatedCall diagnostic
// correctly downgrades to a warning. The RBI SendNonSendable pass must
// mirror that policy: when the underlying
// ApplyExpr::getIsolationCrossing() is preconcurrency on either side,
// the SendingRisksDataRace diagnostic must be a warning, not an error.

@MainActor
class HostComponent: SceneComponent {
  // expected-warning @+3 {{call to main actor-isolated instance method 'helper()' in a synchronous nonisolated context}}
  // expected-warning @+2 {{this will be an error in a future Swift language mode}}
  // expected-warning @+1 {{sending 'self' risks causing data races}}
  override func doWork() { helper() }
  // expected-note @-1 {{'self' is exposed to code in the current isolation context}}
  // expected-note @-2 {{value is exposed to main actor-isolated code}}
  // expected-note @-3 {{sending 'self' to main actor-isolated instance method 'helper()' risks causing data races between main actor-isolated code and code in the current isolation context}}

  // expected-note @+1 {{calls to instance method 'helper()' from outside of its actor context are implicitly asynchronous}}
  private func helper() {}
}

// Second pattern: a non-Sendable value is sent into the same kind of
// preconcurrency apply (the @MainActor helper called from the
// ObjC-override-treated-as-nonisolated context) and then used afterward
// in the original (nonisolated) context. The use-after-send shape is
// emitted by UseAfterSendDiagnosticEmitter::getBehaviorLimit() — the
// second of the two emitter classes that observe an ApplyExpr isolation
// crossing and must mirror the AST-level preconcurrency-downgrade
// policy.

class NonSendable {}

func nonisolatedUse(_: NonSendable) {}

@MainActor
class UseAfterSendComponent: SceneComponent {
  // expected-note @+1 {{'self' is exposed to code in the current isolation context}}
  override func doWork() {
    let x = NonSendable()
    // expected-warning @+5 {{call to main actor-isolated instance method 'consume' in a synchronous nonisolated context}}
    // expected-warning @+4 {{this will be an error in a future Swift language mode}}
    // expected-warning @+3 {{sending 'x' risks causing data races}}
    // expected-warning @+2 {{sending 'self' risks causing data races}}
    // expected-note    @+1 {{value is exposed to main actor-isolated code}}
    consume(x)
    // expected-note @-1 {{sending 'x' to main actor-isolated instance method 'consume' risks causing data races between main actor-isolated and local nonisolated uses}}
    // expected-note @-2 {{sending 'self' to main actor-isolated instance method 'consume' risks causing data races between main actor-isolated code and code in the current isolation context}}
    nonisolatedUse(x)
    // expected-note @-1 {{access can happen concurrently}}
  }

  // expected-note @+1 {{calls to instance method 'consume' from outside of its actor context are implicitly asynchronous}}
  private func consume(_: NonSendable) {}
}
