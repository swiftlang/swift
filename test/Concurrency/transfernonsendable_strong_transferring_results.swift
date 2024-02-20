// RUN: %target-swift-frontend -emit-sil -disable-experimental-parser-round-trip -disable-availability-checking -enable-experimental-feature TransferringArgsAndResults -verify -enable-experimental-feature RegionBasedIsolation %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

struct NonSendableStruct {
  var first = NonSendableKlass()
  var second = NonSendableKlass()
}

func useValue<T>(_ t: T) {}
func getAny() -> Any { fatalError() }

actor Custom {
}

@globalActor
struct CustomActor {
    static var shared: Custom {
        return Custom()
    }
}

@MainActor func transferToMainIndirect<T>(_ t: T) async {}
@CustomActor func transferToCustomIndirect<T>(_ t: T) async {}
@MainActor func transferToMainDirect(_ t: NonSendableKlass) async {}
@CustomActor func transferToCustomDirect(_ t: NonSendableKlass) async {}
func useValueIndirect<T>(_ t: T) {}
func useValueDirect(_ t: NonSendableKlass) {}

func transferValueDirect(_ x: transferring NonSendableKlass) {}
func transferValueIndirect<T>(_ x: transferring T) {}

func transferResult() -> transferring NonSendableKlass { NonSendableKlass() }
func transferResultWithArg(_ x: NonSendableKlass) -> transferring NonSendableKlass { NonSendableKlass() }
func transferResultWithTransferringArg(_ x: transferring NonSendableKlass) -> transferring NonSendableKlass { NonSendableKlass() }
func transferResultWithTransferringArg2(_ x: transferring NonSendableKlass, _ y: NonSendableKlass) -> transferring NonSendableKlass { NonSendableKlass() }
func transferResultWithTransferringArg2Throwing(_ x: transferring NonSendableKlass, _ y: NonSendableKlass) throws -> transferring NonSendableKlass { NonSendableKlass() }

func transferAsyncResult() async -> transferring NonSendableKlass { NonSendableKlass() }
func transferAsyncResultWithArg(_ x: NonSendableKlass) async -> transferring NonSendableKlass { NonSendableKlass() }
func transferAsyncResultWithTransferringArg(_ x: transferring NonSendableKlass) async -> transferring NonSendableKlass { NonSendableKlass() }
func transferAsyncResultWithTransferringArg2(_ x: transferring NonSendableKlass, _ y: NonSendableKlass) async -> transferring NonSendableKlass { NonSendableKlass() }
func transferAsyncResultWithTransferringArg2Throwing(_ x: transferring NonSendableKlass, _ y: NonSendableKlass) async throws -> transferring NonSendableKlass { NonSendableKlass() }

@MainActor var globalNonSendableKlass = NonSendableKlass()

/////////////////
// MARK: Tests //
/////////////////

func simpleTest() async {
  let x = NonSendableKlass()
  let y = transferResultWithArg(x)
  await transferToMainDirect(x)
  useValue(y)
}

// Since y is transfered, we should emit the error on useValue(x). We generally
// emit the first seen error on a path, so if we were to emit an error on
// useValue(y), we would have emitted that error.
func simpleTest2() async {
  let x = NonSendableKlass() // expected-note {{variable defined here}}
  let y = transferResultWithArg(x)
  await transferToMainDirect(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{'x' is transferred from nonisolated caller to main actor-isolated callee. Later uses in caller could race with potential uses in callee}}
  useValue(y)
  useValue(x) // expected-note {{access here could race}}
}

// Make sure that later errors with y can happen.
func simpleTest3() async {
  let x = NonSendableKlass()
  let y = transferResultWithArg(x) // expected-note {{variable defined here}}
  await transferToMainDirect(x)
  await transferToMainDirect(y) // expected-warning {{transferring 'y' may cause a race}}
  // expected-note @-1 {{'y' is transferred from nonisolated caller to main actor-isolated callee}}
  useValue(y) // expected-note {{access here could race}}
}

func transferResult() async -> transferring NonSendableKlass {
  let x = NonSendableKlass() // expected-note {{variable defined here}}
  await transferToMainDirect(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{'x' is transferred from nonisolated caller to main actor-isolated callee. Later uses in caller could race with potential uses in callee}}
  return x // expected-note {{access here could race}}
}

func transferInAndOut(_ x: transferring NonSendableKlass) -> transferring NonSendableKlass {
  x
}


func transferReturnArg(_ x: NonSendableKlass) -> transferring NonSendableKlass {
  return x // expected-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
}

// This is safe since we are returning the whole tuple fresh. In contrast,
// (transferring NonSendableKlass, transferring NonSendableKlass) would not be
// safe if we ever support that.
func transferReturnArgTuple(_ x: transferring NonSendableKlass) -> transferring (NonSendableKlass, NonSendableKlass) {
  return (x, x)
}
