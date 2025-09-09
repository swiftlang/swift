// RUN: %empty-directory(%t)

// A swift 5 module /without/ concurrency checking
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/PreconcurrencyUnchecked.swiftmodule -module-name PreconcurrencyUnchecked %S/Inputs/transfernonsendable_preconcurrency_unchecked.swift -target %target-swift-5.1-abi-triple -swift-version 5

// A swift 5 module /with/ concurrency checking
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/PreconcurrencyChecked.swiftmodule -module-name PreconcurrencyChecked %S/Inputs/transfernonsendable_preconcurrency_checked.swift -target %target-swift-5.1-abi-triple -swift-version 5 -strict-concurrency=complete

// Test swift 5 with strict concurrency
// RUN: %target-swift-frontend -swift-version 5 %s -emit-sil -o /dev/null -verify -verify-additional-prefix swift-5- -parse-as-library -I %t -strict-concurrency=complete -target %target-swift-5.1-abi-triple

// Test swift 6
// RUN: %target-swift-frontend -swift-version 6 %s -emit-sil -o /dev/null -verify -verify-additional-prefix swift-6- -parse-as-library -I %t -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -swift-version 6 %s -emit-sil -o /dev/null -verify -verify-additional-prefix swift-6- -parse-as-library -I %t -target %target-swift-5.1-abi-triple -enable-upcoming-feature NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// README: This test is meant to test the interaction of transfernonsendable and
// preconcurrency. Please only keep such tests in this file.

////////////////////////
// MARK: Declarations //
////////////////////////

@preconcurrency import PreconcurrencyUnchecked
import PreconcurrencyChecked

typealias PreCUncheckedNonSendableKlass = PreconcurrencyUnchecked.NonSendableKlass
typealias PreCUncheckedExplicitlyNonSendableKlass = PreconcurrencyUnchecked.ExplicitlyNonSendableKlass
typealias PostCUncheckedNonSendableKlass = PreconcurrencyChecked.NonSendableKlass

actor CustomActorInstance {}

@globalActor
struct CustomActor {
  static let shared = CustomActorInstance()
}

func transferToNonIsolated<T>(_ t: T) async {}
@MainActor func transferToMain<T>(_ t: T) async {}
@CustomActor func transferToCustom<T>(_ t: T) async {}
func useValue<T>(_ t: T) {}
func useValueAsync<T>(_ t: T) async {}
@MainActor func useValueMain<T>(_ t: T) {}
@MainActor func mainActorFunction() {}

////////////////////////////////////
// MARK: Use After Transfer Tests //
////////////////////////////////////

// In swift 5, this should be squelched and should emit a warning in swift 6.
func testPreconcurrencyImplicitlyNonSendable() async {
  let x = PreCUncheckedNonSendableKlass()
  await transferToMain(x)
  useValue(x)
}

// In swift 5 and swift 6, this should be a warning.
func testPreconcurrencyExplicitlyNonSendable() async {
  let x = PreCUncheckedExplicitlyNonSendableKlass()
  await transferToMain(x)
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-2 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-swift-6-warning @-3 {{sending 'x' risks causing data races}}
  // expected-swift-6-note @-4 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x)
  // expected-swift-5-note @-1 {{access can happen concurrently}}
  // expected-swift-6-note @-2 {{access can happen concurrently}}
}

// In swift 5 this is a warning and in swift 6 this is an error.
func testNormal() async {
  let x = PostCUncheckedNonSendableKlass()
  await transferToMain(x)
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-6-error @-2 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-3 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-swift-6-note @-4 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-swift-5-note {{access can happen concurrently}}
  // expected-swift-6-note @-1 {{access can happen concurrently}}
}

func testErrorOnNonExactValue() async {
  let x = PreCUncheckedNonSendableKlass()
  let y = (x, x)

  await transferToMain(y)
  useValue(x)
}

func testNoErrorIfUseInSameRegionLater() async {
  let x = PreCUncheckedNonSendableKlass()
  let y = (x, x)
  // We squelch since we are sending x.
  await transferToMain(x)
  useValue(y)
}

////////////////////////////////
// MARK: Never Transfer Tests //
////////////////////////////////

func testNeverTransfer(_ x: PreCUncheckedNonSendableKlass) async {
  await transferToMain(x)
}

func testNeverTransferExplicit(_ x: PreCUncheckedExplicitlyNonSendableKlass) async {
  await transferToMain(x)
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-2 {{sending task-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-swift-6-warning @-3 {{sending 'x' risks causing data races}}
  // expected-swift-6-note @-4 {{sending task-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and task-isolated uses}}
}

func testNeverTransferNormal(_ x: PostCUncheckedNonSendableKlass) async {
  await transferToMain(x)
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-2 {{sending task-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-swift-6-error @-3 {{sending 'x' risks causing data races}}
  // expected-swift-6-note @-4 {{sending task-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and task-isolated uses}}
}

func testNeverTransferInexactMatch(_ x: (PreCUncheckedNonSendableKlass, PreCUncheckedNonSendableKlass)) async {
  await transferToMain(x)
}

func testNeverTransferInexactMatchExplicit(_ x: (PreCUncheckedExplicitlyNonSendableKlass, PreCUncheckedExplicitlyNonSendableKlass)) async {
  await transferToMain(x)
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-2 {{sending task-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-swift-6-warning @-3 {{sending 'x' risks causing data races}}
  // expected-swift-6-note @-4 {{sending task-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and task-isolated uses}}
}


