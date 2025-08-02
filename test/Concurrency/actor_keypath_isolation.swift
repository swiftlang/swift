// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -strict-concurrency=complete -emit-sil -o /dev/null -verify  -verify-additional-prefix swift5- -swift-version 5 %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -strict-concurrency=complete -emit-sil -o /dev/null -verify -verify-additional-prefix swift6- -swift-version 6 %s

// REQUIRES: concurrency

class Box {
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

    @preconcurrency @MainActor var unsafeGlobActor_mutable : Int = 0
    @preconcurrency @MainActor let unsafeGlobActor_immutable : Int = 0

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

    _ = (\Door.letBox).appending(path: \Box?.?.size)
    // expected-swift5-warning@-1 {{cannot form key path to actor-isolated property 'letBox'; this is an error in the Swift 6 language mode}}
    // expected-swift6-error@-2 {{cannot form key path to actor-isolated property 'letBox'}}

    _ = (\Door.varBox).appending(path:  // expected-error {{cannot form key path to actor-isolated property 'varBox'}}
                                       \Box.size)

}

func tryKeyPathsFromAsync() async {
    _ = \Door.unsafeGlobActor_immutable
    _ = \Door.unsafeGlobActor_mutable
    // expected-swift5-warning@-1 {{cannot form key path to main actor-isolated property 'unsafeGlobActor_mutable'; this is an error in the Swift 6 language mode}}
    // expected-swift6-error@-2 {{cannot form key path to main actor-isolated property 'unsafeGlobActor_mutable'}}
}

func tryNonSendable() {
    _ = \Door.letDict[0]
    // expected-swift5-warning@-1 {{cannot form key path to actor-isolated property 'letDict'; this is an error in the Swift 6 language mode}}
    // expected-swift6-error@-2 {{cannot form key path to actor-isolated property 'letDict'}}
    _ = \Door.varDict[0] // expected-error {{cannot form key path to actor-isolated property 'varDict'}}
    _ = \Door.letBox!.size
    // expected-swift5-warning@-1 {{cannot form key path to actor-isolated property 'letBox'; this is an error in the Swift 6 language mode}}
    // expected-swift6-error@-2 {{cannot form key path to actor-isolated property 'letBox'}}
}

func tryKeypaths() {
    _ = \Door.unsafeGlobActor_immutable
    _ = \Door.unsafeGlobActor_mutable
    // expected-swift5-warning@-1 {{cannot form key path to main actor-isolated property 'unsafeGlobActor_mutable'; this is an error in the Swift 6 language mode}}
    // expected-swift6-error@-2 {{cannot form key path to main actor-isolated property 'unsafeGlobActor_mutable'}}

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

    _ = \Door.globActor_mutable
    // expected-swift5-warning@-1 {{cannot form key path to main actor-isolated property 'globActor_mutable'; this is an error in the Swift 6 language mode}}
    // expected-swift6-error@-2 {{cannot form key path to main actor-isolated property 'globActor_mutable'}}
    _ = \Door.[0] // expected-error{{cannot form key path to actor-isolated subscript 'subscript(_:)'}}
    _ = \Door.["hello"]
    // expected-swift5-warning@-1 {{cannot form key path to main actor-isolated subscript 'subscript(_:)'; this is an error in the Swift 6 language mode}}
    // expected-swift6-error@-2 {{cannot form key path to main actor-isolated subscript 'subscript(_:)'}}
}

// Make sure that these diagnostics have a source location.
do {
  struct S1 {
    @MainActor
    var prop: Int { get {} }
  }
  @dynamicMemberLookup struct S2 {
    subscript(dynamicMember keyPath: KeyPath<S1, Int>) -> Int { get {} }
  }
  @dynamicMemberLookup struct S3 {
    subscript(dynamicMember keyPath: KeyPath<S2, Int>) -> Int { get {} }
  }

  nonisolated func test(s3: S3) {
    let _ = s3.prop
    // expected-swift5-warning@-1:15 {{cannot form key path that captures non-sendable type 'KeyPath<S1, Int>'; this is an error in the Swift 6 language mode}}
    // expected-swift5-warning@-2:16 {{cannot form key path to main actor-isolated property 'prop'; this is an error in the Swift 6 language mode}}
    // expected-swift6-error@-3:16 {{cannot form key path to main actor-isolated property 'prop'}}
  }
}
do {
  struct S1 {
    var prop: Int { get {} }
  }
  @dynamicMemberLookup struct S2 {
    @MainActor
    subscript(dynamicMember keyPath: KeyPath<S1, Int>) -> Int { get {} }
    // expected-note@-1 {{'subscript(dynamicMember:)' declared here}}
  }
  @dynamicMemberLookup struct S3 {
    subscript(dynamicMember keyPath: KeyPath<S2, Int>) -> Int { get {} }
  }

  nonisolated func test(s3: S3) {
    let _ = s3.prop // s3[dynamicMember: \.[dynamicMember: \.prop]]
    // expected-swift5-warning@-1:15 {{cannot form key path that captures non-sendable type 'KeyPath<S1, Int>'; this is an error in the Swift 6 language mode}}
    // expected-swift5-warning@-2:15 {{cannot form key path to main actor-isolated subscript 'subscript(dynamicMember:)'; this is an error in the Swift 6 language mode}}
    // expected-swift6-error@-3:15 {{cannot form key path to main actor-isolated subscript 'subscript(dynamicMember:)'}}
  }
}
