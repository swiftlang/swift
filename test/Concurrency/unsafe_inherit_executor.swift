// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -emit-sil -o /dev/null -verify -disable-availability-checking %s
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -emit-sil -o /dev/null -verify -disable-availability-checking %s -strict-concurrency=targeted
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -emit-sil -o /dev/null -verify -disable-availability-checking %s -strict-concurrency=complete
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -emit-sil -o /dev/null -verify -disable-availability-checking %s -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: swift_feature_RegionBasedIsolation

// expected-error @+1 {{non-async functions cannot inherit an executor}}
@_unsafeInheritExecutor
func testNonAsync() {}

@_unsafeInheritExecutor
func testAsync() async {}
// expected-warning@-1{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

struct A {
  // expected-error @+1 {{@_unsafeInheritExecutor may only be used on 'func' declarations}}
  @_unsafeInheritExecutor
  init() async {}

  // expected-error @+1 {{non-async functions cannot inherit an executor}}
  @_unsafeInheritExecutor
  func testNonAsync() {}

  @_unsafeInheritExecutor
  func testAsync() async {}
  // expected-warning@-1{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}
}


class NonSendableObject {
  var property = 0
}

@_unsafeInheritExecutor
func useNonSendable(object: NonSendableObject) async {}
// expected-warning@-1{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead; this is an error in the Swift 6 language mode}}{{35:1-24=}}{{36:46-46=, isolation: isolated (any Actor)? = #isolation}}

actor MyActor {
  var object = NonSendableObject()
  func foo() async {
    await useNonSendable(object: self.object)
  }
}

// Note: the tests below are line-number-sensitive.
func inheritsIsolationProperly(isolation: isolated (any Actor)? = #isolation) async { }

// @_unsafeInheritExecutor does not work with #isolation
@_unsafeInheritExecutor
func unsafeCallerA(x: Int) async {
  // expected-warning@-1{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  await inheritsIsolationProperly()
  // expected-error@-1{{#isolation (introduced by a default argument) cannot be used within an '@_unsafeInheritExecutor' function}}{{50:1-24=}}{{51:26-26=, isolation: isolated (any Actor)? = #isolation}}
}

@_unsafeInheritExecutor
func unsafeCallerB() async {
  // expected-warning@-1{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  await inheritsIsolationProperly(isolation: #isolation)
  // expected-error@-1{{#isolation cannot be used within an '@_unsafeInheritExecutor' function}}{{58:1-24=}}{{59:20-20=isolation: isolated (any Actor)? = #isolation}}
}

@_unsafeInheritExecutor
func unsafeCallerC(x: Int, fn: () -> Void, fn2: () -> Void) async {
  // expected-warning@-1{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  await inheritsIsolationProperly()
  // expected-error@-1{{#isolation (introduced by a default argument) cannot be used within an '@_unsafeInheritExecutor' function}}{{66:1-24=}}{{67:28-28=, isolation: isolated (any Actor)? = #isolation, }}
}

@_unsafeInheritExecutor
func unsafeCallerAvoidsNewLoop(x: some AsyncSequence<Int, Never>) async throws {
  // expected-warning@-1{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  for try await _ in x { }
}

// -------------------------------------------------------------------------
// Type checker hack to use _unsafeInheritExecutor_-prefixed versions of
// some concurrency library functions.
// -------------------------------------------------------------------------

enum TL {
  @TaskLocal
  static var string: String = "<undefined>"
}

enum MyError: Error {
case fail
}

protocol P {}

@_unsafeInheritExecutor
func unsafeCallerAvoidsNewLoop() async throws {
  // expected-warning@-1{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  _ = await withUnsafeContinuation { (continuation: UnsafeContinuation<Int, Never>) in
    continuation.resume(returning: 5)
  }

  _ = try await withUnsafeThrowingContinuation { (continuation: UnsafeContinuation<Int, any Error>) in
    continuation.resume(returning: 5)
  }

  _ = await withCheckedContinuation { (continuation: CheckedContinuation<Int, Never>) in
    continuation.resume(returning: 5)
  }

  _ = try await withCheckedThrowingContinuation { (continuation: CheckedContinuation<Int, any Error>) in
    continuation.resume(returning: 5)
  }

  _ = await withTaskCancellationHandler {
    5
  } onCancel: {
  }

  TL.$string.withValue("hello") {
    print(TL.string)
  }

  try await TL.$string.withValue("hello") {
    try await Task.sleep(nanoseconds: 500)
    print(TL.string)
  }

  func operation() async throws -> Int { 7 }
  try await TL.$string.withValue("hello", operation: operation)

  _ = await withDiscardingTaskGroup(returning: Int.self) { group in
    group.addTask {
      print("hello")
    }

    return 5
  }

  _ = try await withThrowingDiscardingTaskGroup(returning: Int.self) { group in
    group.addTask {
      print("hello")
    }

    return 5
  }

  _ = await withTaskExecutorPreference(nil) {
    print("hello")
  }

  _ = await withTaskGroup(of: Int.self, returning: Int.self) { group in
    group.addTask {
      return 5
    }

    return 5
  }

  _ = try await withThrowingTaskGroup(of: Int.self, returning: Int.self) { group in
    group.addTask {
      throw MyError.fail
    }

    throw MyError.fail
  }
}

@_unsafeInheritExecutor
func unsafeClockCaller(
  specificClock: ContinuousClock,
  genericClock: some Clock,
  existentialClock: any Clock,
  existentialCompositionClock: any P & Clock,
) async throws {
  // expected-warning@-6{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  _ = try! await specificClock.measure {
    try await Task.sleep(nanoseconds: 500)
  }

  _ = try! await genericClock.measure {
    try await Task.sleep(nanoseconds: 500)
  }

  _ = try! await existentialClock.measure {
    try await Task.sleep(nanoseconds: 500)
  }

  _ = try! await existentialCompositionClock.measure {
    try await Task.sleep(nanoseconds: 500)
  }
}

@_unsafeInheritExecutor
func _unsafeInheritExecutor_hacky() async { }
// expected-warning@-1{{'@_unsafeInheritExecutor' is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}
