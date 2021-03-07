// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -enable-experimental-distributed
// REQUIRES: concurrency

distributed actor D {

  func hello() {} // ok
  func helloAsync() async {} // ok
  func helloAsyncThrows() async throws {} // ok

  distributed func distHello() { } // ok
  distributed func distHelloAsync() async { } // ok
  distributed func distHelloThrows() throws { } // ok
  distributed func distHelloAsyncThrows() async throws { } // ok
}

func test_outside(distributed: D) async throws {
  distributed.distHello() // expected-error{{call is 'async' but is not marked with 'await'}}
  // expected-error@-1{{call can throw but is not marked with 'try'}}
  // expected-note@-2{{did you mean to use 'try'?}}
  // expected-note@-3{{did you mean to disable error propagation?}}
  // expected-note@-4{{did you mean to handle error as optional value?}}
  try distributed.distHello() // expected-error{{call is 'async' but is not marked with 'await'}}
  await distributed.distHello() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  try await distributed.distHello() // ok

  distributed.distHelloAsync()// expected-error{{call is 'async' but is not marked with 'await'}}
  // expected-error@-1{{call is 'async' but is not marked with 'await'}} // FIXME: error is reported twice
  // expected-error@-2{{call can throw but is not marked with 'try'}}
  // expected-note@-3{{did you mean to use 'try'?}}
  // expected-note@-4{{did you mean to disable error propagation?}}
  // expected-note@-5{{did you mean to handle error as optional value?}}
  try distributed.distHelloAsync() // expected-error{{call is 'async' but is not marked with 'await'}}
  // expected-error@-1{{call is 'async' but is not marked with 'await'}} // FIXME: error is reported twice
  await distributed.distHelloAsync() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  try await distributed.distHelloAsync() // ok

  distributed.distHelloThrows() // expected-error{{call is 'async' but is not marked with 'await'}}
  // expected-error@-1{{call can throw but is not marked with 'try'}}
  // expected-note@-2{{did you mean to use 'try'?}}
  // expected-note@-3{{did you mean to disable error propagation?}}
  // expected-note@-4{{did you mean to handle error as optional value?}}
  try distributed.distHelloThrows() // expected-error{{call is 'async' but is not marked with 'await'}}
  await distributed.distHelloThrows() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  try await distributed.distHelloThrows() // ok

  distributed.distHelloAsyncThrows() // expected-error{{call is 'async' but is not marked with 'await'}}
  // expected-error@-1{{call is 'async' but is not marked with 'await'}} // FIXME: this is double reported
  // expected-error@-2{{call can throw but is not marked with 'try'}}
  // expected-note@-3{{did you mean to use 'try'?}}
  // expected-note@-4{{did you mean to disable error propagation?}}
  // expected-note@-5{{did you mean to handle error as optional value?}}
  try distributed.distHelloAsyncThrows() // expected-error{{call is 'async' but is not marked with 'await'}}
  // expected-error@-1{{call is 'async' but is not marked with 'await'}}  // FIXME: error is reported twice
  await distributed.distHelloAsyncThrows() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  try await distributed.distHelloAsyncThrows() // ok

  // special: the actorAddress may always be referred to
  _ = distributed.actorAddress // ok
  _ = distributed.actorTransport // ok
}

