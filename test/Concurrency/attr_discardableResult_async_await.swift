// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

// https://github.com/apple/swift/issues/60276

@discardableResult @MainActor
func mainActorAsyncDiscardable() async -> Int { 0 }

func consumesMainActorAsyncDiscardable() async {
  await mainActorAsyncDiscardable() // ok
}

// https://github.com/swiftlang/swift/issues/83463

@MainActor
@discardableResult
func returnsDiscardableFunc() -> () -> Void { return {} }

@MainActor
func testDiscardsSyncFuncWithImplicitSendableConversion() {
    returnsDiscardableFunc()
}

@MainActor
@discardableResult
func mainActorAsyncReturnsDiscardableFunc() async -> () -> Void { return {} }

@MainActor
func discardsAsyncFuncWithImplicitSendableConversion() async {
  await mainActorAsyncReturnsDiscardableFunc()
}
