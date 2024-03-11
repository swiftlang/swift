// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation -disable-availability-checking -verify %s -o /dev/null -parse-as-library

// Tests specific to global actors.

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

class Klass {}

struct NonSendableStruct {
  var first = Klass()
  var second = Klass()
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

@MainActor func transferToMain<T>(_ t: T) {}
@CustomActor func transferToCustom<T>(_ t: T) {}

@MainActor var globalKlass = Klass()

/////////////////
// MARK: Tests //
/////////////////

@MainActor func testTransferGlobalActorGuardedValue() async {
  // TODO: Global actor error needed.
  await transferToCustom(globalKlass) // expected-warning {{main actor-isolated value of type 'Klass' transferred to global actor 'CustomActor'-isolated context}}
}

@MainActor func testTransferGlobalActorGuardedValueWithlet() async {
  let x = globalKlass
  await transferToCustom(x) // expected-warning {{main actor-isolated value of type 'Klass' transferred to global actor 'CustomActor'-isolated context}}
}

@MainActor func testTransferGlobalActorGuardedValueWithlet(_ k: Klass) async {
  globalKlass = k
  await transferToCustom(k) // expected-warning {{main actor-isolated value of type 'Klass' transferred to global actor 'CustomActor'-isolated context; later accesses to value could race}}
}
