// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path=%t/RAD.swiftmodule -module-name=RAD -enable-experimental-feature RuntimeDiscoverableAttrs -disable-availability-checking %S/Inputs/runtime_attrs.swift
// RUN: %target-swift-frontend -primary-file %s -emit-ir -I %t -swift-version 5 -enable-experimental-feature RuntimeDiscoverableAttrs -disable-availability-checking | %IRGenFileCheck %s

// REQUIRES: asserts
// REQUIRES: OS=macosx
// REQUIRES: concurrency

// CHECK: @"$s28runtime_attributes_on_actors9TestActorC15asyncExternallyyyKcvpfaAA17FlagForAsyncFuncsHF"
// CHECK: @"$s28runtime_attributes_on_actors9TestActorC11doSomethingyyYaKcvpfaAA17FlagForAsyncFuncsHF"
// CHECK: @"$s28runtime_attributes_on_actors9TestActorC11doSomethingyySiYacvpfaAA17FlagForAsyncFuncsHF"
// CHECK: @"$s28runtime_attributes_on_actors9TestActorC11doSomething_1xySi_SaySiGztYacvpfaAA17FlagForAsyncFuncsHF"
// CHECK: @"$s28runtime_attributes_on_actors13globalAsyncFnSaySSGyYacvpfaAA07FlagForF5FuncsHF"

// CHECK: @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVHa" = internal constant
// CHECK-SAME: i32 5
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors9TestActorC15asyncExternallyyyKcvpfaAA17FlagForAsyncFuncsHF"
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors9TestActorC11doSomethingyyYaKcvpfaAA17FlagForAsyncFuncsHF"
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors9TestActorC11doSomethingyySiYacvpfaAA17FlagForAsyncFuncsHF"
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors9TestActorC11doSomething_1xySi_SaySiGztYacvpfaAA17FlagForAsyncFuncsHF"
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors13globalAsyncFnSaySSGyYacvpfaAA07FlagForF5FuncsHF"

import Distributed

@runtimeMetadata
struct FlagForAsyncFuncs {
  init<Act>(attachedTo: (Act) async throws -> Void) {}
  init<Act>(attachedTo: (Act, Int, [Int]) async throws -> Void) {}
  init<Act>(attachedTo: (Act, Int) async -> Void) {}
  init(attachedTo: () async -> [String]) {}
}

distributed actor TestActor {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  // CHECK-LABEL: define hidden swiftcc void @"$s28runtime_attributes_on_actors9TestActorC15asyncExternallyyyKcvpfaAA17FlagForAsyncFuncs"(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs distributed func asyncExternally() throws {}

  // CHECK-LABEL: define hidden swiftcc void @"$s28runtime_attributes_on_actors9TestActorC11doSomethingyyYaKcvpfaAA17FlagForAsyncFuncs"(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs distributed func doSomething() async throws {}

  // CHECK-LABEL: define hidden swiftcc void @"$s28runtime_attributes_on_actors9TestActorC11doSomethingyySiYacvpfaAA17FlagForAsyncFuncs"(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs nonisolated func doSomething(_: Int) async {}

  // CHECK-LABEL: define hidden swiftcc void @"$s28runtime_attributes_on_actors9TestActorC11doSomething_1xySi_SaySiGztYacvpfaAA17FlagForAsyncFuncs"(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs distributed func doSomething(_: Int, x: [Int]) async {}
}
