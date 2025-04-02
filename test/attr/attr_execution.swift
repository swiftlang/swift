// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature ExecutionAttribute

// REQUIRES: concurrency
// REQUIRES: swift_feature_ExecutionAttribute

@execution(something) func invalidAttr() async {} // expected-error {{unknown option 'something' for attribute 'execution'}}

@concurrent @execution(caller) func mutipleAttrs() async {}
// expected-error@-1 {{global function 'mutipleAttrs()' has multiple actor-isolation attributes (@execution(caller) and @concurrent)}}

do {
  @execution(caller) struct S {}
  // expected-error@-1 {{'@execution(caller)' attribute cannot be applied to this declaration}}

  func f(@execution(caller) param: Int) {}
  // expected-error@-1 {{'@execution(caller)' attribute cannot be applied to this declaration}}
}

@concurrent func nonAsync1() {}
// expected-error@-1 {{cannot use '@concurrent' on non-async global function 'nonAsync1()'}}

@execution(caller) func nonAsync2() {}
// expected-error@-1 {{cannot use '@execution(caller)' on non-async global function 'nonAsync2()'}}

@concurrent func testGlobal() async {} // Ok

struct Test {
  @concurrent init() {}
  // expected-error@-1 {{cannot use '@concurrent' on non-async initializer 'init()'}}

  @concurrent init(async: Void) async {}

  @concurrent func member() {}
  // expected-error@-1 {{cannot use '@concurrent' on non-async instance method 'member()'}}

  @concurrent func member() async {} // Ok

  @concurrent var syncP: Int {
  // expected-error@-1 {{cannot use '@concurrent' on non-async property 'syncP'}}
    get {}
  }
  @concurrent var asyncP: Int {
    get async {}
  }

  // expected-error@+1 {{cannot use '@execution(caller)' on non-async subscript 'subscript(sync:)'}}
  @execution(caller) subscript(sync _: Int) -> Bool {
    @concurrent get { false }
    // expected-error@-1 {{@concurrent' attribute cannot be applied to this declaration}}
    @concurrent set { }
    // expected-error@-1 {{@concurrent' attribute cannot be applied to this declaration}}
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
    // expected-error@-1 {{cannot use '@execution(caller)' on non-async property 'syncP'}}

    @execution(caller) var asyncP: Int { get async }
  }
}

struct TestAttributeCollisions {
  @concurrent nonisolated func testNonIsolated() async {}

  @concurrent func test(arg: isolated MainActor) async {}
  // expected-error@-1 {{cannot use '@concurrent' on instance method 'test(arg:)' because it has an isolated parameter: 'arg'}}
  @concurrent subscript(test arg: isolated MainActor) -> Int {
  // expected-error@-1 {{cannot use '@concurrent' on subscript 'subscript(test:)' because it has an isolated parameter: 'arg'}}
    get async {}
  }

  @concurrent func testIsolationAny(arg: @isolated(any) () -> Void) async {}
  // expected-error@-1 {{cannot use '@concurrent' on instance method 'testIsolationAny(arg:)' because it has a dynamically isolated parameter: 'arg'}}
  @concurrent subscript(testIsolationAny arg: @isolated(any) () -> Void) -> Int {
  // expected-error@-1 {{cannot use '@concurrent' on subscript 'subscript(testIsolationAny:)' because it has a dynamically isolated parameter: 'arg'}}
    get async {}
  }

  @MainActor @concurrent func testGlobalActor() async {}
  // expected-warning @-1 {{instance method 'testGlobalActor()' has multiple actor-isolation attributes (@MainActor and @concurrent)}}

  @execution(caller) nonisolated func testNonIsolatedCaller() async {} // Ok
  @MainActor @execution(caller) func testGlobalActorCaller() async {}
  // expected-warning@-1 {{instance method 'testGlobalActorCaller()' has multiple actor-isolation attributes (@MainActor and @execution(caller))}}
  @execution(caller) func testCaller(arg: isolated MainActor) async {}
  // expected-error@-1 {{cannot use '@execution(caller)' on instance method 'testCaller(arg:)' because it has an isolated parameter: 'arg'}}
  
  @concurrent @Sendable func test(_: @Sendable () -> Void, _: sending Int) async {} // Ok
  @execution(caller) @Sendable func testWithSendableCaller(_: @Sendable () -> Void, _: sending Int) async {} // Ok
}

@MainActor
protocol P {
  func test() async
}

struct InfersMainActor : P {
  @concurrent func test() async {}
}

@MainActor
struct IsolatedType {
  @concurrent func test() async {}
}

_ = { @execution(caller) in // Ok
}

_ = { @concurrent in // Ok
}

_ = { @MainActor @concurrent in
  // expected-error@-1 {{cannot use '@concurrent' because function type is isolated to a global actor 'MainActor'}}
}

_ = { @concurrent () -> Int in
  // expected-error@-1 {{'@concurrent' on non-async closure}}
}

_ = { @execution(caller) (x: isolated (any Actor)?) in
  // expected-error@-1 {{cannot use '@execution(caller)' together with an isolated parameter}}
}
