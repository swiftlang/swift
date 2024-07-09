// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -disable-availability-checking %s
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -disable-availability-checking %s -strict-concurrency=targeted
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -disable-availability-checking %s -strict-concurrency=complete
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -disable-availability-checking %s -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: asserts

// expected-error @+1 {{non-async functions cannot inherit an executor}}
@_unsafeInheritExecutor
func testNonAsync() {}

@_unsafeInheritExecutor
func testAsync() async {}
// expected-warning@-1{{@_unsafeInheritExecutor attribute is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

struct A {
  // expected-error @+1 {{@_unsafeInheritExecutor may only be used on 'func' declarations}}
  @_unsafeInheritExecutor
  init() async {}

  // expected-error @+1 {{non-async functions cannot inherit an executor}}
  @_unsafeInheritExecutor
  func testNonAsync() {}

  @_unsafeInheritExecutor
  func testAsync() async {}
  // expected-warning@-1{{@_unsafeInheritExecutor attribute is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}
}


class NonSendableObject {
  var property = 0
}

@_unsafeInheritExecutor
func useNonSendable(object: NonSendableObject) async {}
// expected-warning@-1{{@_unsafeInheritExecutor attribute is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead; this is an error in the Swift 6 language mode}}{{35:1-24=}}{{36:46-46=, isolation: isolated (any Actor)? = #isolation}}

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
  // expected-warning@-1{{@_unsafeInheritExecutor attribute is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  await inheritsIsolationProperly()
  // expected-error@-1{{#isolation (introduced by a default argument) cannot be used within an '@_unsafeInheritExecutor' function}}{{50:1-24=}}{{51:26-26=, isolation: isolated (any Actor)? = #isolation}}
t 6)
}

@_unsafeInheritExecutor
func unsafeCallerB() async {
  // expected-warning@-1{{@_unsafeInheritExecutor attribute is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  await inheritsIsolationProperly(isolation: #isolation)
  // expected-error@-1{{#isolation cannot be used within an '@_unsafeInheritExecutor' function}}{{58:1-24=}}{{59:20-20=isolation: isolated (any Actor)? = #isolation}}
}

@_unsafeInheritExecutor
func unsafeCallerC(x: Int, fn: () -> Void, fn2: () -> Void) async {
  // expected-warning@-1{{@_unsafeInheritExecutor attribute is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  await inheritsIsolationProperly()
  // expected-error@-1{{#isolation (introduced by a default argument) cannot be used within an '@_unsafeInheritExecutor' function}}{{66:1-24=}}{{67:28-28=, isolation: isolated (any Actor)? = #isolation, }}
}

@_unsafeInheritExecutor
func unsafeCallerAvoidsNewLoop(x: some AsyncSequence<Int, Never>) async throws {
  // expected-warning@-1{{@_unsafeInheritExecutor attribute is deprecated; consider an 'isolated' parameter defaulted to '#isolation' instead}}

  for try await _ in x { }
}
