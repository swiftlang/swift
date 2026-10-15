// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify -verify-additional-prefix ni- %s -o /dev/null -parse-as-library
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify -verify-additional-prefix ni-ns- %s -o /dev/null -parse-as-library -enable-upcoming-feature NonisolatedNonsendingByDefault

// README: This is testing specific patterns around global actors that are
// slightly different in between swift 5 and swift 6. The normal global actor
// test is in swift 5, so any tests that work with swift 5 need to be there.

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}
final class SendableKlass : Sendable {}

actor CustomActorInstance {}

@globalActor
struct CustomActor {
  static let shared = CustomActorInstance()
}

func merge<T, U>(_ t: T, _ u: U) {}
func transferToNonIsolated<T>(_ t: T) async {}
@MainActor func transferToMainActor<T>(_ t: T) async {}
@MainActor func transferToMainActor2<T, U>(_ t: T, _ u: U) async {}
@CustomActor func transferToCustomActor<T>(_ t: T) async {}
func useValue<T>(_ t: T) {}
func useValueAsync<T>(_ t: T) async {}
@MainActor func useValueMainActor<T>(_ t: T) {}
@MainActor func mainActorFunction() {}

var booleanFlag: Bool { false }
@MainActor var mainActorIsolatedGlobal = NonSendableKlass()
@CustomActor var customActorIsolatedGlobal = NonSendableKlass()

/////////////////
// MARK: Tests //
/////////////////

@MainActor func synchronousActorIsolatedClosureError() async {
  let closure = { @MainActor @Sendable in
    MainActor.assertIsolated()
  }

  let erased: () -> Void = closure

  await useValueAsync(erased) // expected-ni-error {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}}
}

@MainActor func synchronousActorIsolatedFunctionError() async {
  let erased: () -> Void = mainActorFunction

  await useValueAsync(erased) // expected-ni-error {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}}
}

@MainActor func synchronousActorIsolatedGenericFunctionError<T>(_ t: T) async {
  let erased: (T) -> Void = useValueMainActor

  await useValueAsync(erased) // expected-ni-error {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}}
}

@MainActor func synchronousActorIsolatedClassMethodError() async {
  @MainActor class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-ni-error {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}}
}

@MainActor func synchronousActorIsolatedFinalClassMethodError() async {
  @MainActor final class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-ni-error {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}}
}

@concurrent
func nonisolatedToGlobalActorMergedRegionNoError() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()

  merge(x, y)

  switch Int.random(in: 1...3) {
    case 1:
    await transferToMainActor2(x, y)

    case 2:
    let c = { @MainActor in
      useValue(x)
      useValue(y)
    }
    await c()

    // TODO: should this behave similarly?
    // case 3:
    // async let v = transferToMainActor2(x, y)
    // await v

    default:
    fatalError()
  }
}
