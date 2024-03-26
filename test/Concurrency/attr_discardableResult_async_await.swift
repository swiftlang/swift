// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

// https://github.com/apple/swift/issues/60276

@discardableResult @MainActor
func mainActorAsyncDiscardable() async -> Int { 0 }

func consumesMainActorAsyncDiscardable() async {
  await mainActorAsyncDiscardable() // ok
}
