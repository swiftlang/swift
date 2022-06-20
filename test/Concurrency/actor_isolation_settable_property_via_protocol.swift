// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -disable-availability-checking
// RUN: %target-typecheck-verify-swift -I %t  -disable-availability-checking -warn-concurrency -parse-as-library
// REQUIRES: concurrency

actor A1 {
  var pa = 0
  // expected-error@-1{{actor-isolated property 'pa' cannot be used to satisfy settable property protocol requirement}}
  // expected-note@-2{{mutation of this property is only permitted within the actor}}
}

actor A2 {
  var pa: Int {
    // expected-error@-1{{actor-isolated property 'pa' cannot be used to satisfy settable property protocol requirement}}
    get async { 1 }
    set { _ = newValue }
    // expected-error@-1{{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
  }
}

protocol PA: Actor {
  var pa: Int { get set }
}
extension A1: PA {}
extension A2: PA {}

func test_a1(act: A1) async {
  await act.pa = 222
  // expected-error@-1{{actor-isolated property 'pa' can not be mutated from a non-isolated context}}
  // expected-warning@-2{{no 'async' operations occur within 'await' expression}}

  // ==== Access through protocol ----------------------------------------------

  await (act as any PA).pa = 42 // Not allowed since act does not even conform to PA to begin with

  let anyPA: any PA = act
  await anyPA.pa = 777 // Not allowed since act does not even conform to PA to begin with
}
