// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature ExecutionAttribute

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_feature_ExecutionAttribute

@execution(concurrent)
func concurrentTest() async {
}

@execution(caller)
func callerTest() async {
}

@MainActor
func actorIsolated() async {}

let _: @execution(caller) () async -> Void = concurrentTest // Ok
let _: @execution(concurrent) () async -> Void = callerTest // Ok

let _: @MainActor () async -> Void = concurrentTest // Ok
let _: @MainActor () async -> Void = callerTest // Ok

let _: @isolated(any) () async -> Void = concurrentTest // Ok
let _: @isolated(any) () async -> Void = callerTest
// expected-error@-1 {{cannot convert value of type '@execution(caller) () async -> ()' to specified type '@isolated(any) () async -> Void'}}

let _: @execution(caller) () async -> Void = actorIsolated // Ok
let _: @execution(concurrent) () async -> Void = actorIsolated // Ok

func testIsolationErasure(fn: @escaping @isolated(any) () async -> Void) {
  let _: @execution(concurrent) () async -> Void = fn // Ok
  let _: @execution(caller) () async -> Void = fn // Ok
}

func testUpcast(arr: [@execution(caller) () async -> Void]) {
  let _: [() async -> Void] = arr // Ok - collection upcast
  let _: [String: () async -> Void] = ["": arr]
  // expected-error@-1 {{cannot convert value of type '[@execution(caller) () async -> Void]' to expected dictionary value type '() async -> Void'}}
}

// Isolated parameter
func testParameterIsolation(fn: @escaping (isolated (any Actor)?) async -> Void) {
  let _: @execution(caller) () async -> Void = fn
  // expected-error@-1 {{cannot convert value of type '(isolated (any Actor)?) async -> Void' to specified type '@execution(caller) () async -> Void'}}
  let _: @execution(concurrent) () async -> Void = fn
  // expected-error@-1 {{cannot convert value of type '(isolated (any Actor)?) async -> Void' to specified type '() async -> Void'}}
}

// Non-conversion situations
do {
  struct S<T> {
  }
  func test<T>(_: S<T>, _: T.Type) {}
  
  test(S<() async -> Void>(), type(of: callerTest))
  // expected-error@-1 {{cannot convert value of type '(@execution(caller) () async -> ()).Type' to expected argument type '(() async -> Void).Type'}}

  test(S<@execution(caller) () async -> Void>(), type(of: concurrentTest))
  // expected-error@-1 {{cannot convert value of type '(() async -> ()).Type' to expected argument type '(@execution(caller) () async -> Void).Type'}}

  test(S<@MainActor () async -> Void>(), type(of: callerTest))
  // expected-error@-1 {{cannot convert value of type '(@execution(caller) () async -> ()).Type' to expected argument type '(@MainActor () async -> Void).Type'}}

  test(S<@MainActor () async -> Void>(), type(of: concurrentTest))
  // expected-error@-1 {{cannot convert value of type '(() async -> ()).Type' to expected argument type '(@MainActor () async -> Void).Type'}}

  test(S<(isolated (any Actor)?) async -> Void>(), type(of: callerTest))
  // expected-error@-1 {{cannot convert value of type '(@execution(caller) () async -> ()).Type' to expected argument type '((isolated (any Actor)?) async -> Void).Type'}}

  test(S<(isolated (any Actor)?) async -> Void>(), type(of: concurrentTest))
  // expected-error@-1 {{cannot convert value of type '(() async -> ()).Type' to expected argument type '((isolated (any Actor)?) async -> Void).Type'}}

  test(S<@isolated(any) () async -> Void>(), type(of: concurrentTest))
  // expected-error@-1 {{cannot convert value of type '(() async -> ()).Type' to expected argument type '(@isolated(any) () async -> Void).Type'}}
  test(S<@isolated(any) () async -> Void>(), type(of: callerTest))
  // expected-error@-1 {{cannot convert value of type '(@execution(caller) () async -> ()).Type' to expected argument type '(@isolated(any) () async -> Void).Type'}}
}

