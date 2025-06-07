// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

// This test validates how we handle partial applies that are isolated to a
// specific isolation domain (causing isolation crossings to occur).

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

actor Custom {
  var x = NonSendableKlass()
}

@globalActor
struct CustomActor {
    static var shared: Custom {
        return Custom()
    }
}

func useValue<T>(_ t: T) {}
@MainActor func transferToMain<T>(_ t: T) {}
@CustomActor func transferToCustom<T>(_ t: T) {}

var boolValue: Bool { false }

/////////////////
// MARK: Tests //
/////////////////

func doSomething(_ x: NonSendableKlass, _ y: NonSendableKlass) { }

actor ProtectsNonSendable {
  var ns: NonSendableKlass = .init()

  nonisolated func testParameter(_ nsArg: NonSendableKlass) async {
    self.assumeIsolated { isolatedSelf in
      isolatedSelf.ns = nsArg // expected-warning {{sending 'nsArg' risks causing data races}}
      // expected-note @-1 {{task-isolated 'nsArg' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }

  nonisolated func testParameterOutOfLine2(_ nsArg: NonSendableKlass) async {
    let closure: (isolated ProtectsNonSendable) -> () = { isolatedSelf in
      isolatedSelf.ns = nsArg // expected-warning {{sending 'nsArg' risks causing data races}}
      // expected-note @-1 {{task-isolated 'nsArg' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later nonisolated uses}}
    }
    self.assumeIsolated(closure)
    self.assumeIsolated(closure)
  }

  nonisolated func testParameterMergedIntoLocal(_ nsArg: NonSendableKlass) async {
    let l = NonSendableKlass()
    doSomething(l, nsArg)
    self.assumeIsolated { isolatedSelf in
      isolatedSelf.ns = l // expected-warning {{sending 'l' risks causing data races}}
      // expected-note @-1 {{task-isolated 'l' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }

  nonisolated func testLocal() async {
    let l = NonSendableKlass()

    // This is safe since we do not reuse l.
    self.assumeIsolated { isolatedSelf in
      isolatedSelf.ns = l
    }
  }

  nonisolated func testLocal2() async {
    let l = NonSendableKlass()

    // This is not safe since we use l later.
    self.assumeIsolated { isolatedSelf in
      isolatedSelf.ns = l // expected-warning {{sending 'l' risks causing data races}}
      // expected-note @-1 {{'l' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later nonisolated uses}}
    }

    useValue(l) // expected-note {{access can happen concurrently}}
  }
}

func normalFunc_testLocal_1() {
  let x = NonSendableKlass()
  let _ = { @MainActor in
    print(x)
  }
}

func normalFunc_testLocal_2() {
  let x = NonSendableKlass()
  let _ = { @MainActor in
    useValue(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  }
  useValue(x) // expected-note {{access can happen concurrently}}
}

// We error here since we are performing a double transfer.
//
// TODO: Add special transfer use so we can emit a double transfer error
// diagnostic.
func transferBeforeCaptureErrors() async {
  let x = NonSendableKlass()
  await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local nonisolated uses}}
  let _ = { @MainActor in // expected-note {{access can happen concurrently}}
    useValue(x)
  }
}

// TODO: This should have an error. We aren't disambiguating the actors.
func testDifferentIsolationFromSameClassKindPartialApply() async {
  let p1 = ProtectsNonSendable()
  let p2 = ProtectsNonSendable()

  let x = NonSendableKlass()

  let closure: (isolated ProtectsNonSendable) -> () = { isolatedSelf in
    print(x)
  }

  await closure(p1)
  await closure(p2)
}

// TODO: This should have an error. We aren't disambiguating the actors.
func testDifferentIsolationFromSameClassKindPartialApplyFlowSensitive() async {
  let p1 = ProtectsNonSendable()
  let p2 = ProtectsNonSendable()

  let x = NonSendableKlass()

  let closure: (isolated ProtectsNonSendable) -> () = { isolatedSelf in
    print(x)
  }

  if await boolValue {
    await closure(p1)
    await closure(p1)
  } else {
    await closure(p2)
    await closure(p2)
  }
}

// TODO: This should have an error. We aren't disambiguating the actors.
func testDifferentIsolationFromSameClassKindPartialApplyFlowSensitive2() async {
  let p1 = ProtectsNonSendable()
  let p2 = ProtectsNonSendable()

  let x = NonSendableKlass()

  let closure: (isolated ProtectsNonSendable) -> () = { isolatedSelf in
    print(x)
  }

  if await boolValue {
    await closure(p1)
  } else {
    await closure(p2)
  }
  await closure(p2)
}
