// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor D {

  func hello() {} // expected-note{{distributed actor-isolated instance method 'hello()' declared here}}
  func helloAsync() async {} // expected-note{{distributed actor-isolated instance method 'helloAsync()' declared here}}
  func helloAsyncThrows() async throws {} // expected-note{{distributed actor-isolated instance method 'helloAsyncThrows()' declared here}}

  distributed func distHello() { } // ok
  distributed func distHelloAsync() async { } // ok
  distributed func distHelloThrows() throws { } // ok
  distributed func distHelloAsyncThrows() async throws { } // ok
}

func test_not_distributed_funcs(distributed: D) async {
  distributed.hello() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  distributed.helloAsync() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  // expected-error@-1{{actor-isolated instance method 'helloAsync()' cannot be called from outside of the actor}} {{3-3=await }}
  distributed.helloAsyncThrows() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  // expected-error@-1{{actor-isolated instance method 'helloAsyncThrows()' cannot be called from outside of the actor}} // TODO: no need to diagnose this, it is impossible to call anyway
  // expected-error@-2{{call can throw, but it is not marked with 'try' and the error is not handled}} // TODO: no need to diagnose this, it is impossible to call anyway
}

func test_outside(distributed: D) async throws {
  distributed.distHello() // expected-error{{actor-isolated distributed instance method 'distHello()' cannot be called from outside of the actor}}
  // expected-error@-1{{call can throw but is not marked with 'try'}}
  // expected-note@-2{{did you mean to use 'try'?}}
  // expected-note@-3{{did you mean to disable error propagation?}}
  // expected-note@-4{{did you mean to handle error as optional value?}}
  try distributed.distHello() // expected-error{{ctor-isolated distributed instance method 'distHello()' cannot be called from outside of the actor}}
  await distributed.distHello() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  try await distributed.distHello() // ok

  distributed.distHelloAsync()// expected-error{{actor-isolated distributed instance method 'distHelloAsync()' cannot be called from outside of the actor}}
  // expected-error@-1{{call can throw but is not marked with 'try'}}
  // expected-note@-2{{did you mean to use 'try'?}}
  // expected-note@-3{{did you mean to disable error propagation?}}
  // expected-note@-4{{did you mean to handle error as optional value?}}
  try distributed.distHelloAsync() // expected-error{{actor-isolated distributed instance method 'distHelloAsync()' cannot be called from outside of the actor}}
  await distributed.distHelloAsync() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  try await distributed.distHelloAsync() // ok

  distributed.distHelloThrows() // expected-error{{actor-isolated distributed instance method 'distHelloThrows()' cannot be called from outside of the actor}}
  // expected-error@-1{{call can throw but is not marked with 'try'}}
  // expected-note@-2{{did you mean to use 'try'?}}
  // expected-note@-3{{did you mean to disable error propagation?}}
  // expected-note@-4{{did you mean to handle error as optional value?}}
  try distributed.distHelloThrows() // expected-error{{actor-isolated distributed instance method 'distHelloThrows()' cannot be called from outside of the actor}}
  await distributed.distHelloThrows() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  try await distributed.distHelloThrows() // ok

  distributed.distHelloAsyncThrows() // expected-error{{actor-isolated distributed instance method 'distHelloAsyncThrows()' cannot be called from outside of the actor}}
  // expected-error@-1{{call can throw but is not marked with 'try'}}
  // expected-note@-2{{did you mean to use 'try'?}}
  // expected-note@-3{{did you mean to disable error propagation?}}
  // expected-note@-4{{did you mean to handle error as optional value?}}
  try distributed.distHelloAsyncThrows() // expected-error{{actor-isolated distributed instance method 'distHelloAsyncThrows()' cannot be called from outside of the actor}}
  await distributed.distHelloAsyncThrows() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  try await distributed.distHelloAsyncThrows() // ok

  // special: the actorAddress may always be referred to
  _ = distributed.id // ok
  _ = distributed.actorSystem // ok
}

