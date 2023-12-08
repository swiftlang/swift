// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency && asserts

actor Door {
    @MainActor var globActor_mutable : Int = 0
    @MainActor(unsafe) var unsafeGlobActor_mutable : Int = 0
    @MainActor subscript(byName: String) -> Int { 0 }
}

func tryKeyPathsFromAsync() async {
    _ = \Door.unsafeGlobActor_mutable // no warning
}

func tryKeypaths() {
    _ = \Door.unsafeGlobActor_mutable // no warning
    _ = \Door.globActor_mutable // no warning
    _ = \Door.["hello"] // no warning
}

@MainActor func testGlobalActorRefInSameContext() {
  _ = \Door.unsafeGlobActor_mutable // Ok
  _ = \Door.globActor_mutable // Ok
  _ = \Door.["hello"] // Ok
}
