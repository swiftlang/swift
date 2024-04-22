// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/PreconcurrencyUnchecked.swiftmodule -module-name PreconcurrencyUnchecked %S/Inputs/transfernonsendable_preconcurrency_unchecked.swift -disable-availability-checking -swift-version 5
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/PreconcurrencyChecked.swiftmodule -module-name PreconcurrencyChecked %S/Inputs/transfernonsendable_preconcurrency_checked.swift -disable-availability-checking -swift-version 5 -strict-concurrency=complete

// RUN: %target-swift-frontend -swift-version 5 %s -emit-sil -o /dev/null -verify -verify-additional-prefix tns- -parse-as-library -I %t

// REQUIRES: concurrency
// REQUIRES: asserts

// README: This test is meant to test the interaction of transfernonsendable and
// preconcurrency. Please only keep such tests in this file.

////////////////////////
// MARK: Declarations //
////////////////////////

@preconcurrency import PreconcurrencyUnchecked
import PreconcurrencyChecked

typealias PreCUncheckedNonSendableKlass = PreconcurrencyUnchecked.NonSendableKlass
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

/////////////////
// MARK: Tests //
/////////////////

func testWarning() async {
  let x = PreCUncheckedNonSendableKlass()
  await transferToMain(x)
  useValue(x)
}
