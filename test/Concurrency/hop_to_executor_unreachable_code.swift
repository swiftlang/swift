// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -verify -o /dev/null %s
// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -verify -o /dev/null %s -strict-concurrency=targeted
// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -verify -o /dev/null %s -strict-concurrency=complete
// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -verify -o /dev/null %s -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

// Check that the inserted hop-to-executor instructions don't cause a false
// "unreachable code" warning.

@MainActor
func bye() -> Never {
    print("bye")
    fatalError()
}

func testit() async {
    await bye()
}

