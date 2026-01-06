// RUN: %target-swift-frontend -emit-module -emit-module-path %t/GlobalActorIsolatedFunction.swiftmodule -module-name GlobalActorIsolatedFunction -strict-concurrency=complete %S/Inputs/GlobalActorIsolatedFunction.swift
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -disable-availability-checking -verify %s -o /dev/null -parse-as-library -I %t

// README: This is testing that we properly serialize ActorIsolation on
// SILFunctions. We do not print it yet on SILFunctions, but we can observe it
// behaviorally.

// REQUIRES: concurrency
// REQUIRES: asserts

import GlobalActorIsolatedFunction

func useValueAsync<T>(_ t: T) async {}

@MainActor func synchronousActorIsolatedFunctionError() async {
  let erased: () -> Void = mainActorFunction

  await useValueAsync(erased) // expected-error {{sending 'erased' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'erased' to nonisolated global function 'useValueAsync' risks causing data races between nonisolated and main actor-isolated uses}}
}
