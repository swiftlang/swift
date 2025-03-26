// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature ExecutionAttribute

// REQUIRES: concurrency
// REQUIRES: swift_feature_ExecutionAttribute

@execution(something) func invalidAttr() async {} // expected-error {{unknown option 'something' for attribute 'execution'}}

@execution(concurrent) @execution(caller) func mutipleAttrs() async {}
// expected-error@-1 {{duplicate attribute}} expected-note@-1 {{attribute already specified here}}

do {
  @execution(caller) struct S {}
  // expected-error@-1 {{'@execution(caller)' attribute cannot be applied to this declaration}}

  func f(@execution(caller) param: Int) {}
  // expected-error@-1 {{'@execution(caller)' attribute cannot be applied to this declaration}}
}

@execution(concurrent) func nonAsync1() {}
// expected-error@-1 {{cannot use '@execution' on non-async global function 'nonAsync1()'}}

@execution(caller) func nonAsync2() {}
// expected-error@-1 {{cannot use '@execution' on non-async global function 'nonAsync2()'}}

@execution(concurrent) func testGlobal() async {} // Ok

struct Test {
  @execution(concurrent) init() {}
  // expected-error@-1 {{cannot use '@execution' on non-async initializer 'init()'}}

  @execution(concurrent) init(async: Void) async {}

  @execution(concurrent) func member() {}
  // expected-error@-1 {{cannot use '@execution' on non-async instance method 'member()'}}

  @execution(concurrent) func member() async {} // Ok

  @execution(concurrent) var syncP: Int {
  // expected-error@-1 {{cannot use '@execution' on non-async property 'syncP'}}
    get {}
  }
  @execution(concurrent) var asyncP: Int {
    get async {}
  }

  // expected-error@+1 {{cannot use '@execution' on non-async subscript 'subscript(sync:)'}}
  @execution(caller) subscript(sync _: Int) -> Bool {
    @execution(concurrent) get { false }
    // expected-error@-1 {{@execution(concurrent)' attribute cannot be applied to this declaration}}
    @execution(concurrent) set { }
    // expected-error@-1 {{@execution(concurrent)' attribute cannot be applied to this declaration}}
  }
  @execution(caller) subscript(async _: Int) -> Bool {
    get async {}
  }

  @execution(caller) var storedVar: Int
  // expected-error@-1 {{'@execution(caller)' must not be used on stored properties}}
  @execution(caller) let storedLet: Int
  // expected-error@-1 {{'@execution(caller)' must not be used on stored properties}}
}

do {
  class C {
    @execution(caller) deinit {}
    // expected-error@-1 {{'@execution(caller)' attribute cannot be applied to this declaration}}
  }
}

do {
  @execution(caller) func local() async {} // Ok

  protocol P {
    @execution(caller) var syncP: Int { get }
    // expected-error@-1 {{cannot use '@execution' on non-async property 'syncP'}}

    @execution(caller) var asyncP: Int { get async }
  }
}

struct TestAttributeCollisions {
  @execution(concurrent) nonisolated func testNonIsolated() async {}

  @execution(concurrent) func test(arg: isolated MainActor) async {}
  // expected-error@-1 {{cannot use '@execution' on instance method 'test(arg:)' because it has an isolated parameter: 'arg'}}
  @execution(concurrent) subscript(test arg: isolated MainActor) -> Int {
  // expected-error@-1 {{cannot use '@execution' on subscript 'subscript(test:)' because it has an isolated parameter: 'arg'}}
    get async {}
  }

  @execution(concurrent) func testIsolationAny(arg: @isolated(any) () -> Void) async {}
  // expected-error@-1 {{cannot use '@execution' on instance method 'testIsolationAny(arg:)' because it has a dynamically isolated parameter: 'arg'}}
  @execution(concurrent) subscript(testIsolationAny arg: @isolated(any) () -> Void) -> Int {
  // expected-error@-1 {{cannot use '@execution' on subscript 'subscript(testIsolationAny:)' because it has a dynamically isolated parameter: 'arg'}}
    get async {}
  }

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

_ = { @execution(caller) in // Ok
}

_ = { @execution(concurrent) in // Ok
}

_ = { @MainActor @execution(concurrent) in
  // expected-error@-1 {{cannot use '@execution' because function type is isolated to a global actor 'MainActor'}}
}

_ = { @execution(concurrent) () -> Int in
  // expected-error@-1 {{'@execution' on non-async closure}}
}

_ = { @execution(caller) (x: isolated (any Actor)?) in
  // expected-error@-1 {{cannot use '@execution' together with an isolated parameter}}
}
