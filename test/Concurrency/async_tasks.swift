// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

@available(SwiftStdlib 5.5, *)
func someAsyncFunc() async -> String { "" }

struct MyError: Error {}
@available(SwiftStdlib 5.5, *)
func someThrowingAsyncFunc() async throws -> String { throw MyError() }

// ==== Unsafe Continuations ---------------------------------------------------

struct Vegetable {}

func buyVegetables(
  shoppingList: [String],
  // a) if all veggies were in store, this is invoked *exactly-once*
  onGotAllVegetables: ([Vegetable]) -> (),

  // b) if not all veggies were in store, invoked one by one (one or more times)
  onGotVegetable: (Vegetable) -> (),
  // b) if at least one onGotVegetable was called *exactly-once*
  // this is invoked once no more veggies will be emitted
  onNoMoreVegetables: () -> (),
  // c) if no veggies _at all_ were available, this is invoked *exactly once*
  onNoVegetablesInStore: (Error) -> ()
) {}

// returns 1 or more vegetables or throws an error
@available(SwiftStdlib 5.5, *)
func buyVegetables(shoppingList: [String]) async throws -> [Vegetable] {
  try await withUnsafeThrowingContinuation { continuation in
    var veggies: [Vegetable] = []

    buyVegetables(
      shoppingList: shoppingList,
      onGotAllVegetables: { veggies in continuation.resume(returning: veggies) },
      onGotVegetable: { v in veggies.append(v) },
      onNoMoreVegetables: { continuation.resume(returning: veggies) },
      onNoVegetablesInStore: { error in continuation.resume(throwing: error) }
      )
  }
}


@available(SwiftStdlib 5.5, *)
func test_unsafeContinuations() async {
  // the closure should not allow async operations;
  // after all: if you have async code, just call it directly, without the unsafe continuation
  let _: String = withUnsafeContinuation { continuation in // expected-error{{cannot pass function of type '(UnsafeContinuation<String, Never>) async -> Void' to parameter expecting synchronous function type}}
    let s = await someAsyncFunc() // expected-note {{'async' inferred from asynchronous operation used here}}
    continuation.resume(returning: s)
  }

  let _: String = await withUnsafeContinuation { continuation in
    continuation.resume(returning: "")
  }

  // rdar://76475495 - suppress warnings for invalid expressions
  func test_invalid_async_no_warnings() async -> Int {
	  return await withUnsafeContinuation {
		  $0.resume(throwing: 1) // expected-error {{cannot convert value of type 'Int' to expected argument type 'Never'}}
	  }
  }
}

@available(SwiftStdlib 5.5, *)
func test_unsafeThrowingContinuations() async throws {
  let _: String = try await withUnsafeThrowingContinuation { continuation in
    continuation.resume(returning: "")
  }

  let _: String = try await withUnsafeThrowingContinuation { continuation in
    continuation.resume(throwing: MyError())
  }

  // using resume(with:)
  let _: String = try await withUnsafeThrowingContinuation { continuation in
    let result : Result<String, MyError> = .success("")
    continuation.resume(with: result)
  }

  let _: String = try await withUnsafeThrowingContinuation { continuation in
    continuation.resume(with: .failure(MyError()))
  }

  // TODO: Potentially could offer some warnings if we know that a continuation was resumed or escaped at all in a closure?
}

// ==== Detached Tasks ---------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test_detached() async throws {
  let handle = detach() {
    await someAsyncFunc() // able to call async functions
  }

  let result: String = await handle.get()
  _ = result
}

@available(SwiftStdlib 5.5, *)
func test_detached_throwing() async -> String {
  let handle: Task.Handle<String, Error> = detach() {
    try await someThrowingAsyncFunc() // able to call async functions
  }

  do {
    return try await handle.get()
  } catch {
    print("caught: \(error)")
  }
}
