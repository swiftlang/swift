// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -verify -o /dev/null %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -verify -o /dev/null %s -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -verify -o /dev/null %s -strict-concurrency=complete
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -verify -o /dev/null %s -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

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

