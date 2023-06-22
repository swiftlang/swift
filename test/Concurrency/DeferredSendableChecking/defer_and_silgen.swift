// RUN: %target-typecheck-verify-swift -emit-silgen -strict-concurrency=complete -enable-experimental-feature DeferredSendableChecking
// REQUIRES: concurrency
// REQUIRES: OS=macosx

/*
 There are three primary* ways that values can be transferred between isolation domains (i.e. 'sends'):
 A) passed as an argument or result to a function running in a different domain
 B) passed into a Sendable closure
 C) read as a property of an actor from an isolation domain besides that actor
 D) Global variables

 This file (together with defer_and_typecheck_only) tests all four of those sends, ensuring that when silgen is not
 run (i.e. in defer_and_typecheck_only.swift, which is run with -typecheck) no diagnostics are produced,
 but when silgen is run (i.e. in defer_and_silgen.swift, which is run with -emit-silgen) all appropriate
 diagnostics are produced.

 TODO: in the future, we might consider not deferring reads through global variables (case D)
    because ownership is unlikely to help make these safe

 Except for the expected diagnostics and the RUN arguments, this file and defer_and_typecheck_only.swift are identical.

    *actually these are the only ways I can think of, but maybe there are more
 */

class NonSendable {
// expected-note@-1 12{{class 'NonSendable' does not conform to the 'Sendable' protocol}}
    var x = 0
}

let globalNS = NonSendable()

@available(SwiftStdlib 5.1, *)
func takesNS(_ : NonSendable) async {}

@available(SwiftStdlib 5.1, *)
func retsNS() async -> NonSendable { NonSendable() }

@available(SwiftStdlib 5.1, *)
func takesSendableClosure(_ : @Sendable () -> ()) {}

@available(SwiftStdlib 5.1, *)
func callActorFuncsFromNonisolated(a : A, ns : NonSendable) async {
    // Send A: passed from actor isolated to nonisolated

    await a.actorTakesNS(ns)
    //expected-warning@-1{{non-sendable type 'NonSendable' passed in call to actor-isolated instance method 'actorTakesNS' cannot cross actor boundary}}

    _ = await a.actorRetsNS()
    //expected-warning@-1{{on-sendable type 'NonSendable' returned by call to actor-isolated instance method 'actorRetsNS()' cannot cross actor boundary}}
}

@available(SwiftStdlib 5.1, *)
func readActorState(a : A) {
    // Send C: read of an actor's nonsendable state from nonisolated context

    print(a.actorNS);
    //expected-warning@-1{{non-sendable type 'NonSendable' in asynchronous access to actor-isolated property 'actorNS' cannot cross actor boundary}}
}

@available(SwiftStdlib 5.1, *)
actor A {
    let actorNS = NonSendable()

    func actorTakesNS(_ : NonSendable) async {}

    func actorRetsNS() async -> NonSendable { NonSendable() }

    func callNonisolatedFuncsFromActor(ns: NonSendable) async {
        // Send A: passed from nonisolated to actor isolated

        await takesNS(ns)
        //expected-warning@-1{{non-sendable type 'NonSendable' exiting actor-isolated context in call to non-isolated global function 'takesNS' cannot cross actor boundary}}

        _ = await retsNS()
        //expected-warning@-1{{non-sendable type 'NonSendable' returned by call from actor-isolated context to non-isolated global function 'retsNS()' cannot cross actor boundary}}
    }

    func callActorFuncsFromDiffActor(ns : NonSendable, a : A) async {
        // Send A: passed between the isolation of two different actors

        await a.actorTakesNS(ns)
        //expected-warning@-1{{non-sendable type 'NonSendable' passed in call to actor-isolated instance method 'actorTakesNS' cannot cross actor boundary}}

        _ = await a.actorRetsNS()
        //expected-warning@-1{{non-sendable type 'NonSendable' returned by call to actor-isolated instance method 'actorRetsNS()' cannot cross actor boundary}}
    }

    func captureNonSendableInReturnedClosure(ns : NonSendable) -> @Sendable () -> () {
        // Send B: pass nonsendable value to closure that is Sendable because of function return type

        return {print(ns)}
        //expected-warning@-1{{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` closure}}
    }

    func captureNonSendableInPassedClosure(ns : NonSendable)  {
        // Send B: pass nonsendable value to closure that is Sendable because of function param type

        takesSendableClosure({print(ns)})
        //expected-warning@-1{{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` closure}}
    }

    func captureNonSendableInLocalClosure(ns : NonSendable)  {
        // Send B: pass nonsendable value to closure that is Sendable because of local type annotation

        let _ : @Sendable () -> () = {print(ns)}
        //expected-warning@-1{{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` closure}}
    }

    func readOtherActorState(a : A) {
        // Send C: read of an actor's nonsendable state from another actor's context

        print(a.actorNS)
        //expected-warning@-1{{non-sendable type 'NonSendable' in asynchronous access to actor-isolated property 'actorNS' cannot cross actor boundary}}
    }

    func usesGlobalFromActor() {
        // Send D: access global variable

        print(globalNS)
        //expected-warning@-1{{non-sendable type 'NonSendable' in asynchronous access to main actor-isolated let 'globalNS' cannot cross actor boundary}}
    }
}
