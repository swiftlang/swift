// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

func someAsyncFunc() async -> String { "" }

struct MyError: Error {}
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
func buyVegetables(shoppingList: [String]) async throws -> [Vegetable] {
  await try withUnsafeThrowingContinuation { continuation in
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


func test_unsafeContinuations() async {
  // the closure should not allow async operations;
  // after all: if you have async code, just call it directly, without the unsafe continuation
  let _: String = withUnsafeContinuation { continuation in // expected-error{{invalid conversion from 'async' function of type '(UnsafeContinuation<String>) async -> Void' to synchronous function type '(UnsafeContinuation<String>) -> Void'}}
    let s = await someAsyncFunc() // rdar://70610141 for getting a better error message here
    continuation.resume(returning: s)
  }

  let _: String = await withUnsafeContinuation { continuation in
    continuation.resume(returning: "")
  }
}

func test_unsafeThrowingContinuations() async {
  let _: String = await try withUnsafeThrowingContinuation { continuation in
    continuation.resume(returning: "")
  }

  let _: String = await try withUnsafeThrowingContinuation { continuation in
    continuation.resume(throwing: MyError())
  }

  // TODO: Potentially could offer some warnings if we know that a continuation was resumed or escaped at all in a closure?
}

// ==== Detached Tasks ---------------------------------------------------------

func test_detached() async throws {
  let handle = Task.runDetached() {
    await someAsyncFunc() // able to call async functions
  }

  let result: String = await try handle.get()
  _ = result
}

func test_detached_throwing() async -> String {
  let handle: Task.Handle<String> = Task.runDetached() {
    await try someThrowingAsyncFunc() // able to call async functions
  }

  do {
    return await try handle.get()
  } catch {
    print("caught: \(error)")
  }
}
