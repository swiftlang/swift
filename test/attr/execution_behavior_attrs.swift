// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

// FIXME: Bad parser diagnostic on C++ side
nonisolated(something) func invalidAttr() async {} // expected-error {{cannot find 'nonisolated' in scope}}
// expected-error@-1 {{cannot find 'something' in scope}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}}

@concurrent nonisolated(nonsending) func mutipleAttrs() async {}
// expected-error@-1 {{global function 'mutipleAttrs()' has multiple actor-isolation attributes (@concurrent and 'nonisolated(nonsending)')}}

do {
  nonisolated(nonsending) struct S {}
  // expected-error@-1 {{'nonisolated(nonsending)' is only applicable to asynchronous functions, initializers, subscripts and computed properties}}


  func f(nonisolated(nonsending) param: Int) {}
  // expected-error@-1 {{expected parameter name followed by ':'}}
  // expected-error@-2 {{parameter may have at most one of the 'inout', 'borrowing', or 'consuming' specifiers}}
}

@concurrent func nonAsync1() {}
// expected-error@-1 {{cannot use @concurrent on non-async global function 'nonAsync1()'}}

nonisolated(nonsending) func nonAsync2() {}
// expected-error@-1 {{cannot use 'nonisolated(nonsending)' on non-async global function 'nonAsync2()'}}

@concurrent func testGlobal() async {} // Ok

struct Test {
  @concurrent init() {}
  // expected-error@-1 {{cannot use @concurrent on non-async initializer 'init()'}}

  @concurrent init(async: Void) async {}

  @concurrent func member() {}
  // expected-error@-1 {{cannot use @concurrent on non-async instance method 'member()'}}

  @concurrent func member() async {} // Ok

  @concurrent var syncP: Int {
  // expected-error@-1 {{cannot use @concurrent on non-async property 'syncP'}}
    get {}
  }
  @concurrent var asyncP: Int {
    get async {}
  }

  // expected-error@+1 {{cannot use 'nonisolated(nonsending)' on non-async subscript 'subscript(sync:)'}}
  nonisolated(nonsending) subscript(sync _: Int) -> Bool {
    @concurrent get { false }
    // expected-error@-1 {{@concurrent' attribute cannot be applied to this declaration}}
    @concurrent set { }
    // expected-error@-1 {{@concurrent' attribute cannot be applied to this declaration}}
  }
  nonisolated(nonsending) subscript(async _: Int) -> Bool {
    get async {}
  }

  // FIXME: Incorrect quotes due to inconsistent DeclAttribute printing between modifiers and attributes
  nonisolated(nonsending) var storedVar: Int
  // expected-error@-1 {{''nonisolated(nonsending)'' must not be used on stored properties}}
  nonisolated(nonsending) let storedLet: Int
  // expected-error@-1 {{''nonisolated(nonsending)'' must not be used on stored properties}}
}

do {
  class C {
    nonisolated(nonsending) deinit {}
    // expected-error@-1 {{'nonisolated(nonsending)' is only applicable to asynchronous functions, initializers, subscripts and computed properties}}
  }
}

do {
  nonisolated(nonsending) func local() async {} // Ok

  protocol P {
    nonisolated(nonsending) var syncP: Int { get }
    // expected-error@-1 {{cannot use 'nonisolated(nonsending)' on non-async property 'syncP'}}

    nonisolated(nonsending) var asyncP: Int { get async }
  }
}

struct TestAttributeCollisions {
  @concurrent nonisolated func testNonIsolated() async {}

  @concurrent func test(arg: isolated MainActor) async {}
  // expected-error@-1 {{cannot use @concurrent on instance method 'test(arg:)' because it has an isolated parameter: 'arg'}}
  @concurrent subscript(test arg: isolated MainActor) -> Int {
  // expected-error@-1 {{cannot use @concurrent on subscript 'subscript(test:)' because it has an isolated parameter: 'arg'}}
    get async {}
  }

  @concurrent func testIsolationAny(arg: @isolated(any) () -> Void) async {}
  // expected-error@-1 {{cannot use @concurrent on instance method 'testIsolationAny(arg:)' because it has a dynamically isolated parameter: 'arg'}}
  @concurrent subscript(testIsolationAny arg: @isolated(any) () -> Void) -> Int {
  // expected-error@-1 {{cannot use @concurrent on subscript 'subscript(testIsolationAny:)' because it has a dynamically isolated parameter: 'arg'}}
    get async {}
  }

  @MainActor @concurrent func testGlobalActor() async {}
  // expected-warning @-1 {{instance method 'testGlobalActor()' has multiple actor-isolation attributes (@MainActor and @concurrent)}}

  nonisolated(nonsending) nonisolated func testNonIsolatedCaller() async {} // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
  @MainActor nonisolated(nonsending) func testGlobalActorCaller() async {}
  // expected-warning@-1 {{instance method 'testGlobalActorCaller()' has multiple actor-isolation attributes (@MainActor and 'nonisolated(nonsending)')}}
  nonisolated(nonsending) func testCaller(arg: isolated MainActor) async {}
  // expected-error@-1 {{cannot use 'nonisolated(nonsending)' on instance method 'testCaller(arg:)' because it has an isolated parameter: 'arg'}}
  
  @concurrent @Sendable func test(_: @Sendable () -> Void, _: sending Int) async {} // Ok
  @Sendable nonisolated(nonsending) func testWithSendableCaller(_: @Sendable () -> Void, _: sending Int) async {} // Ok
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

_ = { @concurrent in // Ok
}

_ = { @MainActor @concurrent in
  // expected-error@-1 {{cannot use @concurrent because function type is isolated to a global actor 'MainActor'}}
}

_ = { @concurrent () -> Int in
  // expected-error@-1 {{@concurrent on non-async closure}}
}

// Make sure that explicit use of `@concurrent` doesn't interfere with inference of `throws` from the body.
do {
    func acceptsThrowing(_ body: () async throws -> Void) async {
    }

  struct Invocation {
    func throwingFn() async throws {
    }
  }

  func test(invocation: Invocation) async {
    await acceptsThrowing({ @concurrent in try await invocation.throwingFn() }) // Ok
    await acceptsThrowing({ @concurrent [invocation] in try await invocation.throwingFn() }) // Ok

    await acceptsThrowing({ @concurrent in // Ok
      _ = 42
      try await invocation.throwingFn()
    })
  }
}
