// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -swift-version 5 -emit-module-path %t/SerializedDefaultArguments.swiftmodule -module-name SerializedDefaultArguments -enable-experimental-feature IsolatedDefaultValues %S/Inputs/serialized_default_arguments.swift

// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -disable-availability-checking -swift-version 6 -I %t
// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -disable-availability-checking -swift-version 6 -I %t -enable-experimental-feature SendNonSendable -enable-experimental-feature IsolatedDefaultValues

// REQUIRES: concurrency
// REQUIRES: asserts

import SerializedDefaultArguments

@MainActor 
func mainActorCaller() {
  useMainActorDefault()
  useNonisolatedDefault()
}

func nonisolatedCaller() async {
  // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  await useMainActorDefault()

  await useMainActorDefault(mainActorDefaultArg())

  await useNonisolatedDefault()
}
