// RUN: %target-swift-frontend -swift-version 6 -Xllvm -sil-regionbasedisolation-force-use-of-typed-src-errors -emit-sil -o /dev/null %s -verify -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency
// REQUIRES: asserts

// READ THIS: This test exercises typed errors for the src (task-isolated or
// non-task-isolated) side of incompatible region merge diagnostics, while the
// dst side still uses name-based diagnostics. This exercises the _src_type
// diagnostic variants.

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

func useNonSendable<T, U>(_ x: T, _ y: U) {}

@globalActor actor CustomActor {
  static let shared = CustomActor()
}

/////////////////
// MARK: Tests //
/////////////////

// Assign merge: src uses type, dst uses name
@MainActor
struct MainActorWithCustomFieldSrcOnly {
  var mainField: NonSendableKlass? = nil
  @CustomActor var customField: NonSendableKlass? = nil

  init(assign: Void = ()) {
    mainField = customField // expected-error {{assigning global actor 'CustomActor'-isolated value of type '()' to main actor-isolated 'self.mainField' risks causing data races}}
    // expected-note @-1 {{a value of type '()' could become accessible to main actor-isolated code despite remaining accessible to global actor 'CustomActor'-isolated code}}
  }

  // NonisolatedFunction merge: src uses type, dst uses name
  init(nonisolatedFunc: Void = ()) {
    useNonSendable(mainField, customField) // expected-error {{passing global actor 'CustomActor'-isolated value of type 'NonSendableKlass?' and main actor-isolated 'self.mainField' as arguments to global function 'useNonSendable' risks causing data races}}
    // expected-note @-1 {{'self.mainField' could begin referencing a value of type 'NonSendableKlass?' allowing concurrent access to a value of type 'NonSendableKlass?' by main actor-isolated code and global actor 'CustomActor'-isolated code}}
  }
}

// FunctionIsolation: src uses type when name inference is suppressed
actor ActorWithIsolatedInitSrcOnly {
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
