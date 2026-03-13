// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify %s -o /dev/null -parse-as-library

// REQUIRES: concurrency

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

func useValue<T>(_ t: T) {}
func useValueAsync<T>(_ t: T) async {}

/////////////////
// MARK: Tests //
/////////////////

@MainActor
func withCheckedContinuation_1() async -> NonSendableKlass {
  await withCheckedContinuation { continuation in
    continuation.resume(returning: NonSendableKlass())
  }
}

func withCheckedContinuation_1a() async -> NonSendableKlass {
  await withCheckedContinuation { continuation in
    continuation.resume(returning: NonSendableKlass())
  }
}

@MainActor
func withCheckedContinuation_2() async -> NonSendableKlass {
  await withCheckedContinuation { continuation in
    let x = NonSendableKlass()
    continuation.resume(returning: x)
    // expected-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }
}

func withCheckedContinuation_2a() async -> NonSendableKlass {
  await withCheckedContinuation { continuation in
    let x = NonSendableKlass()
    continuation.resume(returning: x)
    // expected-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }
}

@MainActor
func withCheckedContinuation_3() async {
  // x is MainActor isolated since withCheckedContinuation is #isolated.
  let x = await withCheckedContinuation { continuation in
    let x = NonSendableKlass()
    continuation.resume(returning: x)
    // expected-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }

  // Since x is returned as sending from withCheckedContinuation, we can send it
  // further.
  await useValueAsync(x)
}

func withCheckedContinuation_3a() async {
  let x = await withCheckedContinuation { continuation in
    let x = NonSendableKlass()
    continuation.resume(returning: x)
    // expected-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }

  // This is ok since x is disconnected.
  await useValueAsync(x)
}

@MainActor
func withCheckedContinuation_4() async {
  // x is MainActor isolated since withCheckedContinuation is #isolated.
  let y = NonSendableKlass()
  let x = await withCheckedContinuation { continuation in
    continuation.resume(returning: y)
    // expected-error @-1 {{sending 'y' risks causing data races}}
    // expected-note @-2 {{main actor-isolated 'y' is passed as a 'sending' parameter}}
    useValue(y)
  }

  // Since withCheckedContinuation returns value as sending, we can call
  // useValueAsync since it is disconnected.
  await useValueAsync(x)
}

func withCheckedContinuation_4a() async {
  // x is MainActor isolated since withCheckedContinuation is #isolated.
  let y = NonSendableKlass()
  let x = await withCheckedContinuation { continuation in
    continuation.resume(returning: y)
    // expected-error @-1 {{sending 'y' risks causing data races}}
    // expected-note @-2 {{task-isolated 'y' is passed as a 'sending' parameter}}
    useValue(y)
  }
  await useValueAsync(x)
}

@MainActor func testAsyncStream() {
  let (_, continuation) = AsyncStream.makeStream(of: NonSendableKlass.self)

  continuation.yield(NonSendableKlass())
  let x = NonSendableKlass()
  continuation.yield(x) // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

@MainActor func testAsyncStreamContinuation() {
  let _ = AsyncStream(NonSendableKlass.self) { continuation in
    continuation.yield(NonSendableKlass())
    let x = NonSendableKlass()
    continuation.yield(x) // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }
}

@MainActor func testAsyncThrowingStream() {
  let (_, continuation) = AsyncThrowingStream.makeStream(of: NonSendableKlass.self)

  continuation.yield(NonSendableKlass())
  let x = NonSendableKlass()
  continuation.yield(x) // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

@MainActor func testAsyncThrowingStreamContinuation() {
  let _ = AsyncThrowingStream(NonSendableKlass.self) { continuation in
    continuation.yield(NonSendableKlass())
    let x = NonSendableKlass()
    continuation.yield(x) // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }
}

@MainActor
func withUnsafeContinuation_1() async -> NonSendableKlass {
  await withUnsafeContinuation { continuation in
    continuation.resume(returning: NonSendableKlass())
  }
}

func withUnsafeContinuation_1a() async -> NonSendableKlass {
  await withUnsafeContinuation { continuation in
    continuation.resume(returning: NonSendableKlass())
  }
}

@MainActor
func withUnsafeContinuation_2() async -> NonSendableKlass {
  await withUnsafeContinuation { continuation in
    let x = NonSendableKlass()
    continuation.resume(returning: x)
    // expected-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }
}

func withUnsafeContinuation_2a() async -> NonSendableKlass {
  await withUnsafeContinuation { continuation in
    let x = NonSendableKlass()
    continuation.resume(returning: x)
    // expected-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }
}

@MainActor
func withUnsafeContinuation_3() async {
  // x is MainActor isolated since withUnsafeContinuation is #isolated.
  let x = await withUnsafeContinuation { continuation in
    let x = NonSendableKlass()
    continuation.resume(returning: x)
    // expected-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }

  // withUnsafeContinuation returns x as sending, so we can pass it to a
  // nonisolated function.
  await useValueAsync(x)
}

func withUnsafeContinuation_3a() async {
  let x = await withUnsafeContinuation { continuation in
    let x = NonSendableKlass()
    continuation.resume(returning: x)
    // expected-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }
  await useValueAsync(x)
}

@MainActor
func withUnsafeContinuation_4() async {
  // x is MainActor isolated since withUnsafeContinuation is #isolated.
  let y = NonSendableKlass()
  let x = await withUnsafeContinuation { continuation in
    continuation.resume(returning: y)
    // expected-error @-1 {{sending 'y' risks causing data races}}
    // expected-note @-2 {{main actor-isolated 'y' is passed as a 'sending' parameter}}
    useValue(y)
  }

  // Since withUnsafeContinuation returns x as sending, we can use it in a
  // nonisolated function.
  await useValueAsync(x)
}

func withUnsafeContinuation_4a() async {
  // x is MainActor isolated since withUnsafeContinuation is #isolated.
  let y = NonSendableKlass()
  let x = await withUnsafeContinuation { continuation in
    continuation.resume(returning: y)
    // expected-error @-1 {{sending 'y' risks causing data races}}
    // expected-note @-2 {{task-isolated 'y' is passed as a 'sending' parameter}}
    useValue(y)
  }
  await useValueAsync(x)
}

public actor WithCheckedThrowingContinuationErrorAvoidance {
  nonisolated func handle(reply: (Int) -> Void) {}

  // make sure that we do not emit any errors on the following code.
  func noError() async throws -> Int {
    return try await withCheckedThrowingContinuation { continuation in 
      handle { result in
        continuation.resume(returning: result)
      }
    }
  }
}
