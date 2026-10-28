// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-module -swift-version 5 -emit-module-path %t/SerializedDefaultArguments.swiftmodule -module-name SerializedDefaultArguments -enable-upcoming-feature IsolatedDefaultValues %S/Inputs/serialized_default_arguments.swift

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -swift-version 6 -I %t
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -swift-version 5 -I %t -enable-upcoming-feature IsolatedDefaultValues -strict-concurrency=complete

// REQUIRES: concurrency
// REQUIRES: swift_feature_IsolatedDefaultValues

import SerializedDefaultArguments

@MainActor 
func mainActorCaller() {
  useMainActorDefault()
  useNonisolatedDefault()
}

func nonisolatedCaller() async {
  await useMainActorDefault()

  await useMainActorDefault(mainActorDefaultArg())

  await useNonisolatedDefault()
}
