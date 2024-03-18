// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify -enable-upcoming-feature RegionBasedIsolation -enable-experimental-feature TransferringArgsAndResults %s -o /dev/null

// This test validates the behavior of transfer non sendable around ownership
// constructs like non copyable types, consuming/borrowing parameters, and inout
// parameters.

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

class Klass {}
struct KlassBox {
  var data = Klass()
}

actor Custom {
}

@globalActor
struct CustomActor {
    static var shared: Custom {
        return Custom()
    }
}

@MainActor func transferToMain<T>(_ t: T) async {}
@MainActor func consumeTransferToMain<T>(_ t: consuming T) async {}

/////////////////
// MARK: Tests //
/////////////////

func testConsuming(_ x: consuming Klass) async {
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring nonisolated 'x' to main actor-isolated callee could cause races between main actor-isolated and nonisolated uses}}
}

func testConsumingError(_ x: consuming Klass) async {
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring nonisolated 'x' to main actor-isolated callee could cause races between main actor-isolated and nonisolated uses}}
  print(x)
}

@CustomActor func testConsumingErrorGlobalActor(_ x: consuming Klass) async {
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring global actor 'CustomActor'-isolated 'x' to main actor-isolated callee could cause races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
  print(x)
}

func testConsumingUseAfterConsumeError(_ x: consuming Klass) async { // expected-error {{'x' consumed more than once}}
  await consumeTransferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring nonisolated 'x' to main actor-isolated callee could cause races between main actor-isolated and nonisolated uses}}
  // expected-note @-2 {{consumed here}}
  print(x)
  // expected-note @-1 {{consumed again here}}
}

@CustomActor func testConsumingUseAfterConsumeErrorGlobalActor(_ x: consuming Klass) async { // expected-error {{'x' consumed more than once}}
  await consumeTransferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring global actor 'CustomActor'-isolated 'x' to main actor-isolated callee could cause races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
  // expected-note @-2 {{consumed here}}
  print(x)
  // expected-note @-1 {{consumed again here}}
}

func testBorrowing(_ x: borrowing Klass) async { // expected-note {{value is task-isolated since it is in the same region as 'x'}}
  await transferToMain(x) // expected-warning {{task-isolated value of type 'Klass' transferred to main actor-isolated context}}
}

func testBorrowingError(_ x: borrowing Klass) async { // expected-error {{'x' is borrowed and cannot be consumed}}
// expected-note @-1 {{}}
  await transferToMain(x) // expected-warning {{task-isolated value of type 'Klass' transferred to main actor-isolated context}}
  print(x) // expected-note {{consumed here}}
}

@CustomActor func testBorrowingErrorGlobalActor(_ x: borrowing Klass) async { // expected-error {{'x' is borrowed and cannot be consumed}}
// expected-note @-1 {{}}
  await transferToMain(x) // expected-warning {{task-isolated value of type 'Klass' transferred to main actor-isolated context}}
  print(x) // expected-note {{consumed here}}
}

func testInOut(_ x: inout Klass) async {
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // TODO: This is wrong. Should say task isolated!
  // expected-note @-2 {{transferring nonisolated 'x' to main actor-isolated callee could cause races between main actor-isolated and nonisolated uses}}
}

func testInOutError(_ x: inout Klass) async {
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring nonisolated 'x' to main actor-isolated callee}}
  print(x)
}

@CustomActor func testInOutErrorMainActor(_ x: inout Klass) async {
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring global actor 'CustomActor'-isolated 'x' to main actor-isolated callee}}
  print(x)
}

@CustomActor func testInOutErrorMainActor2(_ x: inout Klass) async { // expected-error {{'x' used after consume}}
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring global actor 'CustomActor'-isolated 'x' to main actor-isolated callee}}
  _ = consume x // expected-note {{consumed here}}
} // expected-note {{used here}}