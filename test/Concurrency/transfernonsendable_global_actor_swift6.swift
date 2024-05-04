// RUN: %target-swift-frontend -emit-sil -swift-version 6 -disable-availability-checking -verify %s -o /dev/null -parse-as-library

// README: This is testing specific patterns around global actors that are
// slightly different in between swift 5 and swift 6. The normal global actor
// test is in swift 5, so any tests that work with swift 5 need to be there.

// REQUIRES: concurrency
// REQUIRES: asserts

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

func transferToNonIsolated<T>(_ t: T) async {}
@MainActor func transferToMainActor<T>(_ t: T) async {}
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

  await useValueAsync(erased) // expected-error {{transferring 'erased' may cause a data race}}
  // expected-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
}

@MainActor func synchronousActorIsolatedFunctionError() async {
  let erased: () -> Void = mainActorFunction

  await useValueAsync(erased) // expected-error {{transferring 'erased' may cause a data race}}
  // expected-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
}

@MainActor func synchronousActorIsolatedGenericFunctionError<T>(_ t: T) async {
  let erased: (T) -> Void = useValueMainActor

  await useValueAsync(erased) // expected-error {{transferring 'erased' may cause a data race}}
  // expected-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
}

@MainActor func synchronousActorIsolatedClassMethodError() async {
  @MainActor class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-error {{transferring 'erased' may cause a data race}}
  // expected-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
}

@MainActor func synchronousActorIsolatedFinalClassMethodError() async {
  @MainActor final class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-error {{transferring 'erased' may cause a data race}}
  // expected-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
}
