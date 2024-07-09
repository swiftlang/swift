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

struct A {
  // expected-error @+1 {{@_unsafeInheritExecutor may only be used on 'func' declarations}}
  @_unsafeInheritExecutor
  init() async {}

  // expected-error @+1 {{non-async functions cannot inherit an executor}}
  @_unsafeInheritExecutor
  func testNonAsync() {}

  @_unsafeInheritExecutor
  func testAsync() async {}
}


class NonSendableObject {
  var property = 0
}

@_unsafeInheritExecutor
func useNonSendable(object: NonSendableObject) async {}

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
  await inheritsIsolationProperly()
  // expected-error@-1{{#isolation (introduced by a default argument) cannot be used within an '@_unsafeInheritExecutor' function}}{{47:1-24=}}{{48:26-26=, isolation: isolated (any Actor)? = #isolation}}
}

@_unsafeInheritExecutor
func unsafeCallerB() async {
  await inheritsIsolationProperly(isolation: #isolation)
  // expected-error@-1{{#isolation cannot be used within an '@_unsafeInheritExecutor' function}}{{53:1-24=}}{{54:20-20=isolation: isolated (any Actor)? = #isolation}}
}

@_unsafeInheritExecutor
func unsafeCallerC(x: Int, fn: () -> Void, fn2: () -> Void) async {
  await inheritsIsolationProperly()
  // expected-error@-1{{#isolation (introduced by a default argument) cannot be used within an '@_unsafeInheritExecutor' function}}{{59:1-24=}}{{60:28-28=, isolation: isolated (any Actor)? = #isolation, }}
}

@_unsafeInheritExecutor
func unsafeCallerB(x: some AsyncSequence<Int, Never>) async {
  for await _ in x { }
  // expected-error@-1 2{{#isolation cannot be used within an `@_unsafeInheritExecutor` function}}
}
