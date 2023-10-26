// RUN: %target-swift-frontend  -disable-availability-checking -warn-concurrency -swift-version 6 %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking -warn-concurrency -swift-version 6 %s -emit-sil -o /dev/null -verify -enable-experimental-feature RegionBasedIsolation

// REQUIRES: concurrency && asserts

class Box { // expected-note 3{{class 'Box' does not conform to the 'Sendable' protocol}}
    let size : Int = 0
}

actor Door {
    let immutable : Int = 0
    let letBox : Box? = nil
    let letDict : [Int : Box] = [:]
    let immutableNeighbor : Door? = nil


    var mutableNeighbor : Door? = nil
    var varDict : [Int : Box] = [:]
    var mutable : Int = 0
    var varBox : Box = Box()
    var getOnlyInt : Int {
        get { 0 }
    }

    @MainActor var globActor_mutable : Int = 0
    @MainActor let globActor_immutable : Int = 0

    @MainActor(unsafe) var unsafeGlobActor_mutable : Int = 0
    @MainActor(unsafe) let unsafeGlobActor_immutable : Int = 0

    subscript(byIndex: Int) -> Int { 0 }

    @MainActor subscript(byName: String) -> Int { 0 }

    nonisolated subscript(byIEEE754: Double) -> Int { 0 }
}

func attemptAccess<T, V>(_ t : T, _ f : (T) -> V) -> V {
    return f(t)
}

func tryKeyPathsMisc(d : Door) {
    // as a func
    _ = attemptAccess(d, \Door.mutable) // expected-error {{cannot form key path to actor-isolated property 'mutable'}}
    _ = attemptAccess(d, \Door.immutable)
    _ = attemptAccess(d, \Door.immutableNeighbor?.immutableNeighbor)

    // in combination with other key paths

    _ = (\Door.letBox).appending(path:  // expected-warning {{cannot form key path that accesses non-sendable type 'Box?'}}
                                       \Box?.?.size)

    _ = (\Door.varBox).appending(path:  // expected-error {{cannot form key path to actor-isolated property 'varBox'}}
                                       \Box.size)

}

func tryKeyPathsFromAsync() async {
    _ = \Door.unsafeGlobActor_immutable
    _ = \Door.unsafeGlobActor_mutable // expected-error {{cannot form key path to actor-isolated property 'unsafeGlobActor_mutable'}}
}

func tryNonSendable() {
    _ = \Door.letDict[0] // expected-warning {{cannot form key path that accesses non-sendable type '[Int : Box]'}}
    _ = \Door.varDict[0] // expected-error {{cannot form key path to actor-isolated property 'varDict'}}
    _ = \Door.letBox!.size // expected-warning {{cannot form key path that accesses non-sendable type 'Box?'}}
}

func tryKeypaths() {
    _ = \Door.unsafeGlobActor_immutable
    _ = \Door.unsafeGlobActor_mutable // expected-error {{cannot form key path to actor-isolated property 'unsafeGlobActor_mutable'}}

    _ = \Door.immutable
    _ = \Door.globActor_immutable
    _ = \Door.[4.2]
    _ = \Door.immutableNeighbor?.immutableNeighbor?.immutableNeighbor

    _ = \Door.varBox // expected-error{{cannot form key path to actor-isolated property 'varBox'}}
    _ = \Door.mutable  // expected-error{{cannot form key path to actor-isolated property 'mutable'}}
    _ = \Door.getOnlyInt  // expected-error{{cannot form key path to actor-isolated property 'getOnlyInt'}}
    _ = \Door.mutableNeighbor?.mutableNeighbor?.mutableNeighbor // expected-error 3 {{cannot form key path to actor-isolated property 'mutableNeighbor'}}

    let _ : PartialKeyPath<Door> = \.mutable // expected-error{{cannot form key path to actor-isolated property 'mutable'}}
    let _ : AnyKeyPath = \Door.mutable  // expected-error{{cannot form key path to actor-isolated property 'mutable'}}

    _ = \Door.globActor_mutable // expected-error{{cannot form key path to actor-isolated property 'globActor_mutable'}}
    _ = \Door.[0] // expected-error{{cannot form key path to actor-isolated subscript 'subscript(_:)'}}
    _ = \Door.["hello"] // expected-error {{cannot form key path to actor-isolated subscript 'subscript(_:)'}}
}
