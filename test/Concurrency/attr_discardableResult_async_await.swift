// RUN: %target-typecheck-verify-swift  -disable-availability-checking
// REQUIRES: concurrency

// https://github.com/apple/swift/issues/60276

@discardableResult @MainActor
func mainActorAsyncDiscardable() async -> Int { 0 }

func consumesMainActorAsyncDiscardable() async {
  await mainActorAsyncDiscardable() // ok
}
