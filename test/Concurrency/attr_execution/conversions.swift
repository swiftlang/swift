// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature ExecutionAttribute

// REQUIRES: concurrency
// REQUIRES: swift_feature_ExecutionAttribute

@globalActor
actor MyActor {
  static let shared = MyActor()
}

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

do {
  let _: () -> Void = { @execution(concurrent) in
    // expected-error@-1 {{invalid conversion from 'async' function of type '() async -> Void' to synchronous function type '() -> Void'}}
  }

  func test(_: () -> Void) {}

  test { @execution(caller) in
    // expected-error@-1 {{cannot pass function of type '@execution(caller) () async -> ()' to parameter expecting synchronous function type}}
  }
}

// Converting to `@execution(caller)` function
class NonSendable {}

func testNonSendableDiagnostics(
  globalActor1: @escaping @Sendable @MainActor (NonSendable) async -> Void,
  globalActor2: @escaping @Sendable @MainActor () async -> NonSendable,
  erased1: @escaping @Sendable @isolated(any) (NonSendable) async -> Void,
  erased2: @escaping @Sendable @isolated(any) () async -> NonSendable,
  nonIsolated1: @escaping @Sendable (NonSendable) -> Void,
  nonIsolated2: @escaping @Sendable @execution(concurrent) (NonSendable) async -> Void,
  nonIsolated3: @escaping @Sendable () -> NonSendable,
  nonIsolated4: @escaping @Sendable @execution(concurrent) () async -> NonSendable,
  caller1: @escaping @Sendable @execution(caller) (NonSendable) async -> Void,
  caller2: @escaping @Sendable @execution(caller) () async -> NonSendable
) {
  let _: @execution(caller) (NonSendable) async -> Void = globalActor1 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@MainActor @Sendable (NonSendable) async -> Void' to '@execution(caller) (NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  let _: @execution(caller) () async -> NonSendable = globalActor2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@MainActor @Sendable () async -> NonSendable' to '@execution(caller) () async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: @execution(caller) (NonSendable) async -> Void = erased1 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@isolated(any) @Sendable (NonSendable) async -> Void' to '@execution(caller) (NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  let _: @execution(caller) () async -> NonSendable = erased2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@isolated(any) @Sendable () async -> NonSendable' to '@execution(caller) () async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: @execution(caller) (NonSendable) async -> Void = nonIsolated1 // Ok
  let _: @execution(caller) (NonSendable) async -> Void = nonIsolated2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@Sendable (NonSendable) async -> Void' to '@execution(caller) (NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: @execution(caller) () async -> NonSendable = nonIsolated3 // Ok
  let _: @execution(caller) () async -> NonSendable = nonIsolated4 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@Sendable () async -> NonSendable' to '@execution(caller) () async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: @execution(concurrent) (NonSendable) async -> Void = erased1 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@isolated(any) @Sendable (NonSendable) async -> Void' to '(NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  let _: @execution(concurrent) () async -> NonSendable = erased2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@isolated(any) @Sendable () async -> NonSendable' to '() async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}


  let _: @execution(concurrent) (NonSendable) async -> Void = caller1 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@execution(caller) @Sendable (NonSendable) async -> Void' to '(NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  let _: @execution(concurrent) () async -> NonSendable = caller2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@execution(caller) @Sendable () async -> NonSendable' to '() async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: @MainActor (NonSendable) async -> Void = nonIsolated1 // Ok
  let _: @MainActor (NonSendable) async -> Void = nonIsolated2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@Sendable (NonSendable) async -> Void' to '@MainActor (NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: @MainActor () async -> NonSendable = nonIsolated3 // Ok
  let _: @MainActor () async -> NonSendable = nonIsolated4 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@Sendable () async -> NonSendable' to '@MainActor () async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: @MainActor (NonSendable) async -> Void = caller1 // Ok
  let _: @MainActor () async -> NonSendable = caller2 // Ok

  let _: @MyActor (NonSendable) async -> Void = globalActor1
  // expected-error@-1 {{cannot convert value actor-isolated to 'MainActor' to specified type actor-isolated to 'MyActor'}}
  let _: @MyActor () async -> NonSendable = globalActor2
  // expected-error@-1 {{cannot convert value actor-isolated to 'MainActor' to specified type actor-isolated to 'MyActor'}}
}
