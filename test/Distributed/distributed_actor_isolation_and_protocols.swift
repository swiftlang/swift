// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

// ==== ------------------------------------------------------------------------
// MARK: Protocols

protocol LocalProto {
  func local()
  func localAsync() async
  func localAsyncThrows() async throws
}

protocol WrongDistFuncs {
  distributed func notDistActor() // expected-error{{'distributed' function can only be declared within 'distributed actor'}}
}

protocol MixedProto: DistributedActor {
  func local()
  func localAsync() async
  func localAsyncThrows() async throws

  distributed func dist()
  distributed func distAsync() async
  distributed func distAsyncThrows() async throws
}

protocol DistProto: DistributedActor {
  distributed func dist()
  distributed func distAsync() async
  distributed func distAsyncThrows() async throws
}

// ==== ------------------------------------------------------------------------
// MARK: Actors

distributed actor DA1: LocalProto {
  func local() {}
  // expected-error@-1{{actor-isolated instance method 'local()' cannot be used to satisfy a protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'local()' to make this instance method not isolated to the actor}}
  func localAsync() async {}
  func localAsyncThrows() async throws {}
}

