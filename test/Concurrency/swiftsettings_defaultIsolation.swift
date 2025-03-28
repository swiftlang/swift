// RUN: %target-swift-frontend -enable-experimental-feature SwiftSettings -c -verify -swift-version 6 -verify-additional-prefix nonisolated- -disable-availability-checking %s
// RUN: %target-swift-frontend -enable-experimental-feature SwiftSettings -c -verify -swift-version 6 -verify-additional-prefix main-actor- -disable-availability-checking -DSWIFT_SETTINGS_MAIN_ACTOR %s
// RUN: %target-swift-frontend -default-isolation MainActor -enable-experimental-feature SwiftSettings -c -verify -swift-version 6 -disable-availability-checking -verify-additional-prefix main-actor- %s
// RUN: %target-swift-frontend -default-isolation MainActor -enable-experimental-feature SwiftSettings -c -verify -swift-version 6 -disable-availability-checking -verify-additional-prefix nonisolated- -DSWIFT_SETTINGS_NONISOLATED %s

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_feature_SwiftSettings

#if SWIFT_SETTINGS_MAIN_ACTOR

#SwiftSettings(.defaultIsolation(MainActor.self))

#endif

#if SWIFT_SETTINGS_NONISOLATED

#SwiftSettings(.defaultIsolation(nil))

#endif

class UnspecifiedKlass {}
nonisolated class NonisolatedKlass {}

func unspecifiedFunc<T>(_ t: T) async {}
nonisolated func nonisolatedFunc<T>(_ t: T) async {}
@MainActor func mainActorFunc<T>(_ t: T) async {}

func test1(_ x: UnspecifiedKlass) async {
  await unspecifiedFunc(x)
}

func test2(_ x: NonisolatedKlass) async {
  await unspecifiedFunc(x)
}

func test3(_ x: UnspecifiedKlass) async {
  await nonisolatedFunc(x)
}

func test4(_ x: NonisolatedKlass) async {
  await nonisolatedFunc(x) // expected-main-actor-error {{sending 'x' risks causing data races}}
  // expected-main-actor-note @-1 {{sending main actor-isolated 'x' to nonisolated global function 'nonisolatedFunc' risks causing data races between nonisolated and main actor-isolated uses}}
}

func test5(_ x: UnspecifiedKlass) async {
  await mainActorFunc(x) // expected-nonisolated-error {{sending 'x' risks causing data races}}
  // expected-nonisolated-note @-1 {{sending task-isolated 'x' to main actor-isolated global function 'mainActorFunc' risks causing data races between main actor-isolated and task-isolated uses}}
}

func test6(_ x: NonisolatedKlass) async {
  await mainActorFunc(x) // expected-nonisolated-error {{sending 'x' risks causing data races}}
  // expected-nonisolated-note @-1 {{sending task-isolated 'x' to main actor-isolated global function 'mainActorFunc' risks causing data races between main actor-isolated and task-isolated uses}}
}
