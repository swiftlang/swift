// RUN: %target-typecheck-verify-swift -emit-silgen -strict-concurrency=complete -enable-experimental-feature DeferredSendableChecking
// REQUIRES: concurrency
// REQUIRES: OS=macosx

/*
 This file tests the experimental DeferredSendableChecking feature.

 It checks that when silgen is run, diagnostics resulting from passing non-Sendable arguments to a cross-domain
 call are emitted.

 It's tested code is identical to `defer_and_typecheck_only.swift` but this file expects diagnostics to be emitted and
 that one expects them not to be.
 */

class NonSendable {
// expected-note@-1 3{{class 'NonSendable' does not conform to the 'Sendable' protocol}}
    var x = 0
}

let globalNS = NonSendable()

@available(SwiftStdlib 5.1, *)
func takesNS(_ : NonSendable) async {}

@available(SwiftStdlib 5.1, *)
func callActorFuncsFromNonisolated(a : A, ns : NonSendable) async {
    // Send A: passed from actor isolated to nonisolated

    await a.actorTakesNS(ns)
    //expected-warning@-1{{passing argument of non-sendable type 'NonSendable' into actor-isolated context may introduce data races}}
}

@available(SwiftStdlib 5.1, *)
actor A {
    let actorNS = NonSendable()

    func actorTakesNS(_ : NonSendable) async {}

    func callNonisolatedFuncsFromActor(ns: NonSendable) async {
        // Send A: passed from nonisolated to actor isolated

        await takesNS(ns)
        //expected-warning@-1{{passing argument of non-sendable type 'NonSendable' outside of actor-isolated context may introduce data races}}
    }

    func callActorFuncsFromDiffActor(ns : NonSendable, a : A) async {
        // Send A: passed between the isolation of two different actors

        await a.actorTakesNS(ns)
        //expected-warning@-1{{passing argument of non-sendable type 'NonSendable' into actor-isolated context may introduce data races}}
    }
}
