// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-experimental-feature RegionBasedIsolation -disable-availability-checking -verify %s -o /dev/null -swift-version 6 -enable-experimental-feature TransferringArgsAndResults

// REQUIRES: concurrency
// REQUIRES: asserts

// This test makes sure that all of our warnings are errors in swift6 mode.

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableType {}

@MainActor func transferToMain<T>(_ t: T) async {}
func useValue<T>(_ t: T) {}
func transferValue<T>(_ t: transferring T) {}

/////////////////
// MARK: Tests //
/////////////////

func testIsolationError() async {
  let x = NonSendableType()
  await transferToMain(x) // expected-error {{transferring value of non-Sendable type 'NonSendableType' from nonisolated context to main actor-isolated context; later accesses could race}}
  useValue(x) // expected-note {{access here could race}}
}

func testArgumentError(_ x: NonSendableType) async { // expected-note {{value is task isolated since it is in the same region as 'x'}}
  await transferToMain(x) // expected-error {{task isolated value of type 'NonSendableType' transferred to main actor-isolated context; later accesses to value could race}}
}

func testTransferringArgument(_ x: NonSendableType) async {
  transferValue(x) // expected-error {{task isolated value of type 'NonSendableType' passed as a strongly transferred parameter; later accesses could race}}
}
