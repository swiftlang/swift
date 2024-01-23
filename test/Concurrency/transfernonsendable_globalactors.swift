// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-experimental-feature RegionBasedIsolation -disable-availability-checking -verify -verify-additional-prefix tns-  %s -o /dev/null -parse-as-library

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
  await transferToCustom(globalKlass) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
}

@MainActor func testTransferGlobalActorGuardedValueWithlet() async {
  let x = globalKlass
  await transferToCustom(x) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
}

@MainActor func testTransferGlobalActorGuardedValueWithlet(_ k: Klass) async {
  globalKlass = k
  await transferToCustom(k) // expected-tns-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
}
