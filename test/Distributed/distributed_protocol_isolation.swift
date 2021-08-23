// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

// ==== -----------------------------------------------------------------------
// MARK: Good cases

@available(SwiftStdlib 5.5, *)
protocol DistProtocol: DistributedActor {
  // FIXME(distributed): avoid issuing these warnings, these originate from the call on the DistProtocol where we marked this func as dist isolated,
  func local() -> String
  // (the note appears a few times, because we misuse the call many times)
  // expected-note@-2{{calls to instance method 'local()' from outside of its actor context are implicitly asynchronous}} // FIXME: don't emit this note
  // expected-note@-3{{calls to instance method 'local()' from outside of its actor context are implicitly asynchronous}} // FIXME: don't emit this note
  // expected-note@-4{{calls to instance method 'local()' from outside of its actor context are implicitly asynchronous}} // FIXME: don't emit this note

  distributed func dist() -> String
  distributed func dist(string: String) -> String

  distributed func distAsync() async -> String
  distributed func distThrows() throws -> String
  distributed func distAsyncThrows() async throws -> String
}

@available(SwiftStdlib 5.5, *)
distributed actor SpecificDist: DistProtocol {

  nonisolated func local() -> String { "hi" } // expected-note{{only 'distributed' functions can be called from outside the distributed actor}}

  distributed func dist() -> String { "dist!" }
  distributed func dist(string: String) -> String { string }

  distributed func distAsync() async -> String { "dist!" }
  distributed func distThrows() throws -> String { "dist!" }
  distributed func distAsyncThrows() async throws -> String { "dist!" }

  func inside() async throws {
    _ = self.local() // ok

    _ = self.dist() // ok
    _ = self.dist(string: "") // ok
    _ = await self.distAsync() // ok
    _ = try self.distThrows() // ok
    _ = try await self.distAsyncThrows() // ok
  }
}

@available(SwiftStdlib 5.5, *)
func outside_good(dp: SpecificDist) async throws {
  _ = dp.local() // expected-error{{only 'distributed' functions can be called from outside the distributed actor}}

  _ = try await dp.dist() // implicit async throws
  _ = try await dp.dist(string: "") // implicit async throws
  _ = try await dp.distAsync() // implicit throws
  _ = try await dp.distThrows() // implicit async
  _ = try await dp.distAsyncThrows() // ok
}

@available(SwiftStdlib 5.5, *)
func outside_good_generic<DP: DistProtocol>(dp: DP) async throws {
  _ = dp.local() // expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
  _ = await dp.local() // expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
  // the below warning is expected because we don't apply the "implicitly async" to the not-callable func
  // expected-warning@-2{{no 'async' operations occur within 'await' expression}}

  _ = try dp.local() // expected-error{{only 'distributed' functions can be called from outside the distributed actor}}
  // the below warning is expected because we don't apply the "implicitly throwing" to the not-callable func
  // expected-warning@-2{{no calls to throwing functions occur within 'try' expression}}

  _ = try await dp.dist() // implicit async throws
  _ = try await dp.dist(string: "") // implicit async throws
  _ = try await dp.distAsync() // implicit throws
  _ = try await dp.distThrows() // implicit async
  _ = try await dp.distAsyncThrows() // ok
}

@available(SwiftStdlib 5.5, *)
func outside_good_ext<DP: DistProtocol>(dp: DP) async throws {
  _ = try await dp.dist() // implicit async throws
  _ = try await dp.dist(string: "") // implicit async throws
  _ = try await dp.distAsync() // implicit throws
  _ = try await dp.distThrows() // implicit async
  _ = try await dp.distAsyncThrows() // ok
}

// ==== -----------------------------------------------------------------------
// MARK: Error cases

@available(SwiftStdlib 5.5, *)
protocol ErrorCases: DistributedActor {
  distributed func unexpectedAsyncThrows() -> String
  // expected-note@-1{{protocol requires function 'unexpectedAsyncThrows()' with type '() -> String'; do you want to add a stub?}}
}

@available(SwiftStdlib 5.5, *)
distributed actor BadGreeter: ErrorCases {
  // expected-error@-1{{type 'BadGreeter' does not conform to protocol 'ErrorCases'}}

  distributed func unexpectedAsyncThrows() async throws -> String { "" }
  // expected-note@-1{{candidate is 'async', but protocol requirement is not}}
}
