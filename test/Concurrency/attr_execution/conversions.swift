// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

@globalActor
actor MyActor {
  static let shared = MyActor()
}

@concurrent
func concurrentTest() async {
}

nonisolated(nonsending)
func callerTest() async {
}

@MainActor
func actorIsolated() async {}

let _: nonisolated(nonsending) () async -> Void = concurrentTest // Ok
let _: @concurrent () async -> Void = callerTest // Ok

let _: @MainActor () async -> Void = concurrentTest // Ok
let _: @MainActor () async -> Void = callerTest // Ok

let _: @isolated(any) () async -> Void = concurrentTest // Ok
let _: @isolated(any) () async -> Void = callerTest
// expected-error@-1 {{cannot convert value of type 'nonisolated(nonsending) () async -> ()' to specified type '@isolated(any) () async -> Void'}}

let _: nonisolated(nonsending) () async -> Void = actorIsolated // Ok
let _: @concurrent () async -> Void = actorIsolated // Ok

func testIsolationErasure(fn: @escaping @isolated(any) () async -> Void) {
  let _: @concurrent () async -> Void = fn // Ok
  let _: nonisolated(nonsending) () async -> Void = fn // Ok
}

func testUpcast(arr: [nonisolated(nonsending) () async -> Void]) {
  let _: [() async -> Void] = arr // Ok - collection upcast
  let _: [String: () async -> Void] = ["": arr]
  // expected-error@-1 {{cannot convert value of type '[nonisolated(nonsending) () async -> Void]' to expected dictionary value type '() async -> Void'}}
}

// Isolated parameter
func testParameterIsolation(fn: @escaping (isolated (any Actor)?) async -> Void, caller: nonisolated(nonsending) @escaping (String) async -> Void) {
  let _: nonisolated(nonsending) () async -> Void = fn
  // expected-error@-1 {{cannot convert value of type '(isolated (any Actor)?) async -> Void' to specified type 'nonisolated(nonsending) () async -> Void'}}
  let _: @concurrent () async -> Void = fn
  // expected-error@-1 {{cannot convert value of type '(isolated (any Actor)?) async -> Void' to specified type '() async -> Void'}}

  let _: (isolated (any Actor)?) async -> Void = callerTest // Ok
  let _: (isolated (any Actor)?) -> Void = callerTest
  // expected-error@-1 {{invalid conversion from 'async' function of type 'nonisolated(nonsending) () async -> ()' to synchronous function type '(isolated (any Actor)?) -> Void'}}
  let _: (isolated (any Actor)?) async -> Void = concurrentTest
  // expected-error@-1 {{cannot convert value of type '() async -> ()' to specified type '(isolated (any Actor)?) async -> Void'}}
  let _: (isolated (any Actor)?, Int) async -> Void = callerTest
  // expected-error@-1 {{cannot convert value of type 'nonisolated(nonsending) () async -> ()' to specified type '(isolated (any Actor)?, Int) async -> Void'}}

  let _: (String, isolated any Actor) async -> Void = caller // Ok
  let _: (isolated (any Actor)?, String) async -> Void = caller // Ok
}

// Non-conversion situations
do {
  struct S<T> {
  }
  func test<T>(_: S<T>, _: T.Type) {}
  
  test(S<() async -> Void>(), type(of: callerTest))
  // expected-error@-1 {{cannot convert value of type '(nonisolated(nonsending) () async -> ()).Type' to expected argument type '(() async -> Void).Type'}}

  test(S<nonisolated(nonsending) () async -> Void>(), type(of: concurrentTest))
  // expected-error@-1 {{cannot convert value of type '(() async -> ()).Type' to expected argument type '(nonisolated(nonsending) () async -> Void).Type'}}

  test(S<@MainActor () async -> Void>(), type(of: callerTest))
  // expected-error@-1 {{cannot convert value of type '(nonisolated(nonsending) () async -> ()).Type' to expected argument type '(@MainActor () async -> Void).Type'}}

  test(S<@MainActor () async -> Void>(), type(of: concurrentTest))
  // expected-error@-1 {{cannot convert value of type '(() async -> ()).Type' to expected argument type '(@MainActor () async -> Void).Type'}}

  test(S<(isolated (any Actor)?) async -> Void>(), type(of: callerTest))
  // expected-error@-1 {{cannot convert value of type '(nonisolated(nonsending) () async -> ()).Type' to expected argument type '((isolated (any Actor)?) async -> Void).Type'}}

  test(S<(isolated (any Actor)?) async -> Void>(), type(of: concurrentTest))
  // expected-error@-1 {{cannot convert value of type '(() async -> ()).Type' to expected argument type '((isolated (any Actor)?) async -> Void).Type'}}

  test(S<@isolated(any) () async -> Void>(), type(of: concurrentTest))
  // expected-error@-1 {{cannot convert value of type '(() async -> ()).Type' to expected argument type '(@isolated(any) () async -> Void).Type'}}
  test(S<@isolated(any) () async -> Void>(), type(of: callerTest))
  // expected-error@-1 {{cannot convert value of type '(nonisolated(nonsending) () async -> ()).Type' to expected argument type '(@isolated(any) () async -> Void).Type'}}
}

do {
  let _: () -> Void = { @concurrent in
    // expected-error@-1 {{cannot use @concurrent on non-async closure}}{{none}}
  }
}

// Converting to `nonisolated(nonsending)` function
class NonSendable {}

func testNonSendableDiagnostics(
  globalActor1: @escaping @Sendable @MainActor (NonSendable) async -> Void,
  globalActor2: @escaping @Sendable @MainActor () async -> NonSendable,
  erased1: @escaping @Sendable @isolated(any) (NonSendable) async -> Void,
  erased2: @escaping @Sendable @isolated(any) () async -> NonSendable,
  nonIsolated1: @escaping @Sendable (NonSendable) -> Void,
  nonIsolated2: @escaping @Sendable @concurrent (NonSendable) async -> Void,
  nonIsolated3: @escaping @Sendable () -> NonSendable,
  nonIsolated4: @escaping @Sendable @concurrent () async -> NonSendable,
  caller1: nonisolated(nonsending) @escaping @Sendable (NonSendable) async -> Void,
  caller2: nonisolated(nonsending) @escaping @Sendable () async -> NonSendable
) {
  let _: nonisolated(nonsending) (NonSendable) async -> Void = globalActor1 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@MainActor @Sendable (NonSendable) async -> Void' to 'nonisolated(nonsending) (NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  let _: nonisolated(nonsending) () async -> NonSendable = globalActor2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@MainActor @Sendable () async -> NonSendable' to 'nonisolated(nonsending) () async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: @concurrent (NonSendable) async -> Void = globalActor1 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@MainActor @Sendable (NonSendable) async -> Void' to '(NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  let _: @concurrent () async -> NonSendable = globalActor2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@MainActor @Sendable () async -> NonSendable' to '() async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  
  let _: nonisolated(nonsending) (NonSendable) async -> Void = erased1 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@isolated(any) @Sendable (NonSendable) async -> Void' to 'nonisolated(nonsending) (NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  let _: nonisolated(nonsending) () async -> NonSendable = erased2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@isolated(any) @Sendable () async -> NonSendable' to 'nonisolated(nonsending) () async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: nonisolated(nonsending) (NonSendable) async -> Void = nonIsolated1 // Ok
  let _: nonisolated(nonsending) (NonSendable) async -> Void = nonIsolated2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@Sendable (NonSendable) async -> Void' to 'nonisolated(nonsending) (NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: nonisolated(nonsending) () async -> NonSendable = nonIsolated3 // Ok
  let _: nonisolated(nonsending) () async -> NonSendable = nonIsolated4 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-error@-1 {{cannot convert '@Sendable () async -> NonSendable' to 'nonisolated(nonsending) () async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

  let _: @concurrent (NonSendable) async -> Void = erased1 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@isolated(any) @Sendable (NonSendable) async -> Void' to '(NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  let _: @concurrent () async -> NonSendable = erased2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert '@isolated(any) @Sendable () async -> NonSendable' to '() async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}


  let _: @concurrent (NonSendable) async -> Void = caller1 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert 'nonisolated(nonsending) @Sendable (NonSendable) async -> Void' to '(NonSendable) async -> Void' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
  let _: @concurrent () async -> NonSendable = caller2 // expected-note {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  // expected-warning@-1 {{cannot convert 'nonisolated(nonsending) @Sendable () async -> NonSendable' to '() async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}

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
