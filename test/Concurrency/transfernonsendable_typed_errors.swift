// RUN: %target-swift-frontend -swift-version 6 -Xllvm -sil-regionbasedisolation-force-use-of-typed-errors -emit-sil -o /dev/null %s -verify -verify-additional-prefix ni- -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -swift-version 6 -Xllvm -sil-regionbasedisolation-force-use-of-typed-errors -emit-sil -o /dev/null %s -verify -verify-additional-prefix ni-ns- -target %target-swift-5.1-abi-triple -enable-upcoming-feature NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// READ THIS: This test is only intended to test typed errors that are fallback
// error paths that are only invoked if we can't find a name for the value being
// sent. Since in most cases we are able to infer a name, we do not invoke these
// very often (and in truth in normal compilation we would like to never emit
// them). To make sure that we can at least test them out, this test uses an
// asserts only option that causes us to emit typed errors even when we find a
// name.

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

actor MyActor {}

@MainActor func transferToMain<T>(_ t: T) async {}
func transferToSendingParam<T>(_ x: sending T) {}

/////////////////
// MARK: Tests //
/////////////////

func simpleUseAfterFree() async {
  let x = NonSendableKlass()
  await transferToMain(x) // expected-error {{sending value of non-Sendable type 'NonSendableKlass' risks causing data races}}
  // expected-note @-1 {{sending value of non-Sendable type 'NonSendableKlass' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  print(x) // expected-note {{access can happen concurrently}}
}

func isolatedClosureTest() async {
  let x = NonSendableKlass()
  let _ = { @MainActor in
      print(x) // expected-error {{sending value of non-Sendable type 'NonSendableKlass' risks causing data races}}
      // expected-note @-1 {{sending value of non-Sendable type 'NonSendableKlass' to main actor-isolated closure due to closure capture risks causing races in between main actor-isolated and nonisolated uses}}
  }
  print(x) // expected-note {{access can happen concurrently}}
}

func sendingError() async {
  let x = NonSendableKlass()
  transferToSendingParam(x) // expected-error {{sending value of non-Sendable type 'NonSendableKlass' risks causing data races}}
  // expected-note @-1 {{Passing value of non-Sendable type 'NonSendableKlass' as a 'sending' argument to global function 'transferToSendingParam' risks causing races in between local and caller code}}
  print(x) // expected-note {{access can happen concurrently}}
}

extension MyActor {
  func testNonSendableCaptures(sc: NonSendableKlass) {
    Task {
      _ = self
      _ = sc

      Task { [sc,self] in
        _ = self
        _ = sc

        Task { // expected-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
          _ = sc // expected-note {{closure captures 'self'-isolated 'sc'}}
        }

        Task { // expected-error {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
          _ = sc // expected-note {{closure captures 'self'-isolated 'sc'}}
        }
      }
    }
  }
}

@MainActor
func sendingTransferNonSendableError(_ x: NonSendableKlass) {
  transferToSendingParam(x) // expected-error {{sending value of non-Sendable type 'NonSendableKlass' risks causing data races}}
  // expected-note @-1 {{Passing main actor-isolated value of non-Sendable type 'NonSendableKlass' as a 'sending' parameter to global function 'transferToSendingParam' risks causing races inbetween main actor-isolated uses and uses reachable from 'transferToSendingParam'}}
}

func sendingTransferNonSendableError(_ x: NonSendableKlass) async {
  await transferToMain(x) // expected-error {{sending value of non-Sendable type 'NonSendableKlass' risks causing data races}}
  // expected-note @-1 {{sending task-isolated value of non-Sendable type 'NonSendableKlass' to main actor-isolated global function 'transferToMain' risks causing races in between task-isolated and main actor-isolated uses}}
}

func useNonSendable<T, U>(_ x: T, _ y: U) {}

@globalActor actor CustomActor {
  static let shared = CustomActor()
}

// Assign merge: both sides use types (src=CustomActor-type, dst=MainActor-type)
@MainActor
struct MainActorWithCustomField {
  var mainField: NonSendableKlass? = nil
  @CustomActor var customField: NonSendableKlass? = nil

  init(assign: Void = ()) {
    mainField = customField // expected-error {{assigning global actor 'CustomActor'-isolated value of type '()' to main actor-isolated value of type '()' risks causing data races}}
    // expected-note @-1 {{a value of type '()' could become accessible to main actor-isolated code despite remaining accessible to global actor 'CustomActor'-isolated code}}
  }

  // NonisolatedFunction merge: both sides use types
  init(nonisolatedFunc: Void = ()) {
    useNonSendable(mainField, customField) // expected-error {{passing global actor 'CustomActor'-isolated value of type 'NonSendableKlass?' and main actor-isolated value of type 'NonSendableKlass?' as arguments to global function 'useNonSendable' risks causing data races}}
    // expected-note @-1 {{a value of type 'NonSendableKlass?' could begin referencing a value of type 'NonSendableKlass?' allowing concurrent access to a value of type 'NonSendableKlass?' by main actor-isolated code and global actor 'CustomActor'-isolated code}}
  }
}

// FunctionIsolation: src uses type when name inference fails
actor ActorWithIsolatedInit {
  var localVar: NonSendableKlass
  init(value val: NonSendableKlass) { self.localVar = val }
  init(valueAsync val: NonSendableKlass) async { self.localVar = val }
  nonisolated init(nonisoAsync val: NonSendableKlass, _ c: Int) async {
    if c == 0 {
      await self.init(valueAsync: val) // expected-error {{passing a value of type 'NonSendableKlass' to 'self'-isolated initializer 'init(valueAsync:)' risks causing data races}}
    } else {
      self.init(value: val)
    }
  }
}
