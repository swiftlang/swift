// RUN: %target-typecheck-verify-swift -typecheck -strict-concurrency=complete -enable-experimental-feature DeferredSendableChecking
// REQUIRES: concurrency
// REQUIRES: OS=macosx

/*
 This file tests the experimental DeferredSendableChecking feature.

 It checks that when silgen is NOT run (typecheck only), diagnostics resulting from passing non-Sendable arguments to
 a cross-domain call are NOT emitted.

 It's tested code is identical to `defer_and_silgeny.swift` but this file expects diagnostics to not to be emitted and
 that one expects them to be emitted.
 */

class NonSendable {
    var x = 0
}

let globalNS = NonSendable()

@available(SwiftStdlib 5.1, *)
func takesNS(_ : NonSendable) async {}

@available(SwiftStdlib 5.1, *)
func callActorFuncsFromNonisolated(a : A, ns : NonSendable) async {
    // Send A: passed from actor isolated to nonisolated

    await a.actorTakesNS(ns)
    //deferred-warning@-1{{passing argument of non-sendable type 'NonSendable' into actor-isolated context may introduce data races}}
}

@available(SwiftStdlib 5.1, *)
        actor A {
    let actorNS = NonSendable()

    func actorTakesNS(_ : NonSendable) async {}

    func callNonisolatedFuncsFromActor(ns: NonSendable) async {
        // Send A: passed from nonisolated to actor isolated

        await takesNS(ns)
        //deferred-warning@-1{{passing argument of non-sendable type 'NonSendable' outside of actor-isolated context may introduce data races}}
    }

    func callActorFuncsFromDiffActor(ns : NonSendable, a : A) async {
        // Send A: passed between the isolation of two different actors

        await a.actorTakesNS(ns)
        //deferred-warning@-1{{passing argument of non-sendable type 'NonSendable' into actor-isolated context may introduce data races}}
    }
}
