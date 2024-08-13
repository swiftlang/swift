// RUN: %empty-directory(%t)

// A swift 5 module /without/ concurrency checking
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/PreconcurrencyUnchecked.swiftmodule -module-name PreconcurrencyUnchecked %S/Inputs/transfernonsendable_preconcurrency_unchecked.swift -disable-availability-checking -swift-version 5

// A swift 5 module /with/ concurrency checking
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/PreconcurrencyChecked.swiftmodule -module-name PreconcurrencyChecked %S/Inputs/transfernonsendable_preconcurrency_checked.swift -disable-availability-checking -swift-version 5 -strict-concurrency=complete

// Test swift 5 with strict concurrency
// RUN: %target-swift-frontend -swift-version 5 %s -emit-sil -o /dev/null -verify -verify-additional-prefix swift-5- -parse-as-library -I %t -strict-concurrency=complete -disable-availability-checking

// Test swift 6
// RUN: %target-swift-frontend -swift-version 6 %s -emit-sil -o /dev/null -verify -verify-additional-prefix swift-6- -parse-as-library -I %t -disable-availability-checking

// REQUIRES: concurrency
// REQUIRES: asserts

// README: This test is meant to test the interaction of transfernonsendable,
// preconcurrency, and transferring. Please only keep such tests in this file.

////////////////////////
// MARK: Declarations //
////////////////////////

@preconcurrency import PreconcurrencyUnchecked
import PreconcurrencyChecked // expected-swift-5-no-tns-warning {{add '@preconcurrency' to suppress 'Sendable'-related warnings from module 'PreconcurrencyChecked'}}

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

func transferArg<T>(_ t: sending T) {}

actor MyActor {}
func takeClosure(_ x: sending () -> ()) {}
func takeClosureAndParam<T>(_ x: T, _ y: sending () -> ()) {}

////////////////////////////////////
// MARK: Use After Transfer Tests //
////////////////////////////////////

// In swift 5, this should be squelched and should emit a warning in swift 6.
func testPreconcurrencyImplicitlyNonSendable() async {
  let x = PreCUncheckedNonSendableKlass()
  transferArg(x)
  useValue(x)
}

// In swift 5 and swift 6, this should be a warning.
func testPreconcurrencyExplicitlyNonSendable() async {
  let x = PreCUncheckedExplicitlyNonSendableKlass()
  transferArg(x)

  // expected-swift-5-warning @-2 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-3 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
  // expected-swift-6-warning @-4 {{sending 'x' risks causing data races}}
  // expected-swift-6-note @-5 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(x)
  // expected-swift-5-note @-1 {{access can happen concurrently}}
  // expected-swift-6-note @-2 {{access can happen concurrently}}
}

// In swift 5 this is a warning and in swift 6 this is an error.
func testNormal() async {
  let x = PostCUncheckedNonSendableKlass()
  transferArg(x) // expected-swift-5-no-tns-warning {{passing argument of non-sendable type 'PostCUncheckedNonSendableKlass' (aka 'NonSendableKlass') into main actor-isolated context may introduce data races}}
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-6-error @-2 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-3 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
  // expected-swift-6-note @-4 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(x) // expected-swift-5-note {{access can happen concurrently}}
  // expected-swift-6-note @-1 {{access can happen concurrently}}
}

func testOnlyErrorOnExactValue() async {
  let x = PreCUncheckedNonSendableKlass()
  let y = (x, x)
  // We would squelch this if we transferred it directly. Also we error even
  // though we use x later.
  transferArg(y)
  // expected-swift-5-no-tns-warning @-1 2{{passing argument of non-sendable type '(PreCUncheckedNonSendableKlass, PreCUncheckedNonSendableKlass)' (aka '(NonSendableKlass, NonSendableKlass)') into main actor-isolated context may introduce data races}}
  // expected-swift-5-warning @-2 {{sending 'y' risks causing data races}}
  // expected-swift-5-note @-3 {{'y' used after being passed as a 'sending' parameter; Later uses could race}}
  // expected-swift-6-error @-4 {{sending 'y' risks causing data races}}
  // expected-swift-6-note @-5 {{'y' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(x)
  // expected-swift-5-note @-1 {{access can happen concurrently}}
  // expected-swift-6-note @-2 {{access can happen concurrently}}
}

func testNoErrorIfUseInSameRegionLater() async {
  let x = PreCUncheckedNonSendableKlass()
  let y = (x, x)
  // We squelch since we are sending x.
  transferArg(x)
  useValue(y)
}

////////////////////////////////
// MARK: Never Transfer Tests //
////////////////////////////////

func testNeverTransfer(_ x: PreCUncheckedNonSendableKlass) async {
  transferArg(x)
}

func testNeverTransferExplicit(_ x: PreCUncheckedExplicitlyNonSendableKlass) async {
  transferArg(x) // expected-swift-5-no-tns-warning {{passing argument of non-sendable type 'PreCUncheckedExplicitlyNonSendableKlass' (aka 'ExplicitlyNonSendableKlass') into main actor-isolated context may introduce data races}}
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-2 {{task-isolated 'x' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
  // expected-swift-6-warning @-3 {{sending 'x' risks causing data races}}
  // expected-swift-6-note @-4 {{task-isolated 'x' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
}

func testNeverTransferNormal(_ x: PostCUncheckedNonSendableKlass) async {
  transferArg(x) // expected-swift-5-no-tns-warning {{passing argument of non-sendable type 'PostCUncheckedNonSendableKlass' (aka 'NonSendableKlass') into main actor-isolated context may introduce data races}}
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-2 {{task-isolated 'x' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
  // expected-swift-6-error @-3 {{sending 'x' risks causing data races}}
  // expected-swift-6-note @-4 {{task-isolated 'x' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
}

// Inexact match => normal behavior.
func testNeverTransferInexactMatch(_ x: (PreCUncheckedNonSendableKlass, PreCUncheckedNonSendableKlass)) async {
  transferArg(x) // expected-swift-5-no-tns-warning 2{{passing argument of non-sendable type '(PreCUncheckedNonSendableKlass, PreCUncheckedNonSendableKlass)' (aka '(NonSendableKlass, NonSendableKlass)') into main actor-isolated context may introduce data races}}
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-2 {{task-isolated 'x' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
  // expected-swift-6-error @-3 {{sending 'x' risks causing data races}}
  // expected-swift-6-note @-4 {{task-isolated 'x' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
}

// Inexact match => normal behavior.
func testNeverTransferInexactMatchExplicit(_ x: (PreCUncheckedExplicitlyNonSendableKlass, PreCUncheckedExplicitlyNonSendableKlass)) async {
  transferArg(x) // expected-swift-5-no-tns-warning {{passing argument of non-sendable type '(PreCUncheckedExplicitlyNonSendableKlass, PreCUncheckedExplicitlyNonSendableKlass)' (aka '(ExplicitlyNonSendableKlass, ExplicitlyNonSendableKlass)') into main actor-isolated context may introduce data races}}
  // expected-swift-5-warning @-1 {{sending 'x' risks causing data races}}
  // expected-swift-5-note @-2 {{task-isolated 'x' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
  // expected-swift-6-error @-3 {{sending 'x' risks causing data races}}
  // expected-swift-6-note @-4 {{task-isolated 'x' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
}

////////////////////////////////////////
// MARK: Never Sendable Closure Tests //
////////////////////////////////////////

// In this case, if anything captures PreCUncheckedNonSendableKlass directly, we
// squelch the error due to preconcurrency. If we do not capture directly, we
// emit the error... unfortunately.
func taskIsolatedCaptureInSendingClosureLiteral(_ x: PreCUncheckedNonSendableKlass) {
  Task {
    print(x)
  }

  takeClosure {
    print(x)
  }

  takeClosureAndParam(PreCUncheckedNonSendableKlass()) {
    print(x)
  }

  // This isn't an exact match so we emit the error.
  let y = (x, x)
  Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(y) // expected-note {{closure captures 'y' which is accessible to code in the current task}}
  }

  let z = (x, y)
  Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(y, z) // expected-note @:11 {{closure captures non-Sendable 'y'}}
    // expected-note @-1:14 {{closure captures non-Sendable 'z'}}
  }
}

extension MyActor {
  func actorIsolatedCaptureInSendingClosureLiteral(_ x: PreCUncheckedNonSendableKlass) {
    Task {
      print(x)
    }

    takeClosure {
      print(x)
    }

    takeClosureAndParam(PreCUncheckedNonSendableKlass()) {
      print(x)
    }

    let y = (x, x)
    Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(y) // expected-note {{closure captures 'y' which is accessible to 'self'-isolated code}}
    }

    let z = (x, y)
    Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(y, z) // expected-note @:13 {{closure captures non-Sendable 'y'}}
      // expected-note @-1:16 {{closure captures non-Sendable 'z'}}
    }
  }
}

// Since this is unchecked by marking as explicitly non-Sendable, we respect
// what the importing user wanted and change swift 6 to warn and swift 5 to
// warn.
func taskIsolatedCaptureInSendingClosureLiteral(_ x: PreCUncheckedExplicitlyNonSendableKlass) {
  Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
  }

  takeClosure { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
  }

  takeClosureAndParam(PreCUncheckedExplicitlyNonSendableKlass()) { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
  }

  let y = (x, x)
  Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(y) // expected-note {{closure captures 'y' which is accessible to code in the current task}}
  }

  let z = (x, y)
  Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(y, z) // expected-note @:11 {{closure captures non-Sendable 'y'}}
    // expected-note @-1:14 {{closure captures non-Sendable 'z'}}
  }
}

extension MyActor {
  func actorIsolatedCaptureInSendingClosureLiteral(_ x: PreCUncheckedExplicitlyNonSendableKlass) {
    Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(x) // expected-note {{closure captures 'self'-isolated 'x'}}
    }

    takeClosure { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(x) // expected-note {{closure captures 'self'-isolated 'x'}}
    }

    takeClosureAndParam(PreCUncheckedExplicitlyNonSendableKlass()) { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(x) // expected-note {{closure captures 'self'-isolated 'x'}}
    }

    let y = (x, x)
    Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(y) // expected-note {{closure captures 'y' which is accessible to 'self'-isolated code}}
    }

    let z = (x, y)
    Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(y, z) // expected-note @:13 {{closure captures non-Sendable 'y'}}
      // expected-note @-1:16 {{closure captures non-Sendable 'z'}}
    }
  }
}

// Since this is a swift 6 class, we emit the full error.
func taskIsolatedCaptureInSendingClosureLiteral(_ x: PostCUncheckedNonSendableKlass) {
  Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
  }

  takeClosure { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
  }

  takeClosureAndParam(PostCUncheckedNonSendableKlass()) { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
  }

  let y = (x, x)
  Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(y) // expected-note {{closure captures 'y' which is accessible to code in the current task}}
  }

  let z = (x, y)
  Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(y, z) // expected-note @:11 {{closure captures non-Sendable 'y'}}
    // expected-note @-1:14 {{closure captures non-Sendable 'z'}}
  }
}

extension MyActor {
  func actorIsolatedCaptureInSendingClosureLiteral(_ x: PostCUncheckedNonSendableKlass) {
    Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(x) // expected-note {{closure captures 'self'-isolated 'x'}}
    }

    takeClosure { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(x) // expected-note {{closure captures 'self'-isolated 'x'}}
    }

    takeClosureAndParam(PostCUncheckedNonSendableKlass()) { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(x) // expected-note {{closure captures 'self'-isolated 'x'}}
    }

    let y = (x, x)
    Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(y) // expected-note {{closure captures 'y' which is accessible to 'self'-isolated code}}
    }

    let z = (x, y)
    Task { // expected-swift-6-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      // expected-swift-5-warning @-1 {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(y, z) // expected-note @:13 {{closure captures non-Sendable 'y'}}
      // expected-note @-1:16 {{closure captures non-Sendable 'z'}}
    }
  }
}
