// RUN: %target-swift-frontend -swift-version 6 -Xllvm -sil-regionbasedisolation-force-use-of-typed-dst-errors -emit-sil -o /dev/null %s -verify -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency
// REQUIRES: asserts

// READ THIS: This test exercises typed errors for the dst (actor-isolated)
// side of incompatible region merge diagnostics, while the src side still uses
// name-based diagnostics. This exercises the _dst_type diagnostic variants.

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

// Assign merge: src uses name, dst uses type
@MainActor
struct MainActorWithCustomFieldDstOnly {
  var mainField: NonSendableKlass? = nil
  @CustomActor var customField: NonSendableKlass? = nil

  init(assign: Void = ()) {
    mainField = customField // expected-error {{assigning global actor 'CustomActor'-isolated 'self.customField' to main actor-isolated value of type '()' risks causing data races}}
    // expected-note @-1 {{'self.customField' could become accessible to main actor-isolated code despite remaining accessible to global actor 'CustomActor'-isolated code}}
  }

  // NonisolatedFunction merge: src uses name, dst uses type
  init(nonisolatedFunc: Void = ()) {
    useNonSendable(mainField, customField) // expected-error {{passing global actor 'CustomActor'-isolated 'self.customField' and main actor-isolated value of type 'NonSendableKlass?' as arguments to global function 'useNonSendable' risks causing data races}}
    // expected-note @-1 {{a value of type 'NonSendableKlass?' could begin referencing 'self.customField' allowing concurrent access to 'self.customField' by main actor-isolated code and global actor 'CustomActor'-isolated code}}
  }
}

// FunctionIsolation: ForceTypedDstErrors does not affect the functionisolation
// diagnostic since it only mentions the src value. The named version fires.
actor ActorWithIsolatedInitDstOnly {
  var localVar: NonSendableKlass
  init(value val: NonSendableKlass) { self.localVar = val }
  init(valueAsync val: NonSendableKlass) async { self.localVar = val }
  nonisolated init(nonisoAsync val: NonSendableKlass, _ c: Int) async {
    if c == 0 {
      await self.init(valueAsync: val) // expected-error {{passing 'val' to 'self'-isolated initializer 'init(valueAsync:)' risks causing data races}}
    } else {
      self.init(value: val)
    }
  }
}
