// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

extension Actor {
    func f() -> String { "Life is Study!" }
}

func g<A: Actor>(a: A) async { // expected-note{{where 'A' = 'MA'}}
    print(await a.f())
}

distributed actor MA {
}

func h(ma: MA) async {
    // this would have been a bug, a non distributed instance method might have been called here,
    // so we must not allow for it, because if the actor was remote calling a non-distributed func
    // would result in a hard crash (as there is no local actor to safely call the function on).
    await g(a: ma) // expected-error{{global function 'g(a:)' requires that 'MA' conform to 'Actor'}}
}
