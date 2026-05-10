// RUN: %target-swift-frontend -emit-sil -swift-version 6 %s -o /dev/null -verify

// REQUIRES: concurrency


struct UnavailableSendable {}

@available(*, unavailable)
extension UnavailableSendable: Sendable {}
// expected-note@-1 {{conformance of 'UnavailableSendable' to 'Sendable' has been explicitly marked unavailable here}}

@available(SwiftStdlib 5.1, *)
func checkOpaqueType() -> some Sendable {
  UnavailableSendable()
  // expected-error@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}
}

// Make sure that there is no ambiguity betweeen async and sync versions when the difference is `@Sendable` and `async`.
// `withError` has to be generic and have two overloads because this test is exercising the interaction between
// `@Sendable` subtyping and a performance hack that checks specialization without considering arguments if
// there are only two generic overloads.
do {
  func withError<T>(_ body: () throws -> T) throws -> T {
  }

  func withError<T>(_ body: @Sendable () async throws -> T) async throws -> T {
  }

  func compute() throws {}
  @Sendable func longCompute() async throws {}

  // The context is async here to make sure that the synchronous version of `withError` could still be selected.
  func test() async throws {
    try withError { // Ok
    }

    try await withError {
      // expected-warning@-1 {{no 'async' operations occur within 'await' expression}}
    }

    try withError { // Ok
      try compute()
    }

    try withError { // expected-error {{expression is 'async' but is not marked with 'await'}} expected-note {{call is 'async'}}
      try await longCompute()
    }

    try await withError { // Ok
      try await longCompute()
    }
  }
}
