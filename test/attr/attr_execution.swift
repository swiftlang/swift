// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature ExecutionAttribute

// REQUIRES: concurrency
// REQUIRES: swift_feature_ExecutionAttribute

@execution(something) func invalidAttr() async {} // expected-error {{unknown option 'something' for attribute 'execution'}}

@execution(concurrent) @execution(caller) func mutipleAttrs() async {}
// expected-error@-1 {{duplicate attribute}} expected-note@-1 {{attribute already specified here}}

@execution(concurrent) func nonAsync1() {}
// expected-error@-1 {{cannot use '@execution' on non-async global function 'nonAsync1()'}}

@execution(caller) func nonAsync2() {}
// expected-error@-1 {{cannot use '@execution' on non-async global function 'nonAsync2()'}}

@execution(concurrent) func testGlobal() async {} // Ok

struct Test {
  @execution(concurrent) init() {}
  // expected-error@-1 {{@execution(concurrent) may only be used on 'func' declarations}}

  @execution(concurrent) func member() {}
  // expected-error@-1 {{cannot use '@execution' on non-async instance method 'member()'}}

  @execution(concurrent) func member() async {} // Ok

  // expected-error@+1 {{@execution(caller) may only be used on 'func' declarations}}
  @execution(caller) subscript(a: Int) -> Bool {
    get { false }
    @execution(concurrent) set { }
    // expected-error@-1 {{@execution(concurrent) may only be used on 'func' declarations}}
  }

  @execution(caller) var x: Int
  // expected-error@-1 {{@execution(caller) may only be used on 'func' declarations}}
}

do {
  @execution(caller) func local() async {} // Ok
}

struct TestAttributeCollisions {
  @execution(concurrent) nonisolated func testNonIsolated() async {}

  @execution(concurrent) func test(arg: isolated MainActor) async {}
  // expected-error@-1 {{cannot use '@execution' on instance method 'test(arg:)' because it has an isolated parameter: 'arg'}}

  @execution(concurrent) func testIsolationAny(arg: @isolated(any) () -> Void) async {}
  // expected-error@-1 {{cannot use '@execution' on instance method 'testIsolationAny(arg:)' because it has a dynamically isolated parameter: 'arg'}}

  @MainActor @execution(concurrent) func testGlobalActor() async {}
  // expected-warning @-1 {{instance method 'testGlobalActor()' has multiple actor-isolation attributes ('MainActor' and 'execution(concurrent)')}}

  @execution(caller) nonisolated func testNonIsolatedCaller() async {} // Ok
  @MainActor @execution(caller) func testGlobalActorCaller() async {}
  // expected-warning@-1 {{instance method 'testGlobalActorCaller()' has multiple actor-isolation attributes ('MainActor' and 'execution(caller)')}}
  @execution(caller) func testCaller(arg: isolated MainActor) async {}
  // expected-error@-1 {{cannot use '@execution' on instance method 'testCaller(arg:)' because it has an isolated parameter: 'arg'}}
  
  @execution(concurrent) @Sendable func test(_: @Sendable () -> Void, _: sending Int) async {} // Ok
  @execution(caller) @Sendable func testWithSendableCaller(_: @Sendable () -> Void, _: sending Int) async {} // Ok
}

@MainActor
protocol P {
  func test() async
}

struct InfersMainActor : P {
  @execution(concurrent) func test() async {}
}

@MainActor
struct IsolatedType {
  @execution(concurrent) func test() async {}
}
