// RUN: %target-swift-frontend -emit-sil -swift-version 6 -disable-availability-checking -enable-experimental-feature TransferringArgsAndResults -verify %s -o /dev/null -parse-as-library

// REQUIRES: asserts
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

@MainActor
func withCheckedContinuation_3() async {
  // x is main actor isolated since withCheckedContinuation is #isolated.
  let x = await withCheckedContinuation { continuation in
    let x = NonSendableKlass()
    continuation.resume(returning: x)
    // expected-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }
  await useValueAsync(x)
  // expected-error @-1 {{sending 'x' risks causing data races}}
  // expected-note @-2 {{sending main actor-isolated 'x' to nonisolated global function 'useValueAsync' risks causing data races between nonisolated and main actor-isolated uses}}
}

@MainActor
func withCheckedContinuation_4() async {
  // x is main actor isolated since withCheckedContinuation is #isolated.
  let y = NonSendableKlass()
  let x = await withCheckedContinuation { continuation in
    continuation.resume(returning: y)
    // expected-error @-1 {{sending 'y' risks causing data races}}
    // expected-note @-2 {{main actor-isolated 'y' is passed as a 'sending' parameter}}
    useValue(y)
  }
  await useValueAsync(x)
  // expected-error @-1 {{sending 'x' risks causing data races}}
  // expected-note @-2 {{sending main actor-isolated 'x' to nonisolated global function 'useValueAsync' risks causing data races between nonisolated and main actor-isolated uses}}
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
