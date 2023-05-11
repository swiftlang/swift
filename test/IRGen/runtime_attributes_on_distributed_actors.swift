// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path=%t/RAD.swiftmodule -module-name=RAD -enable-experimental-feature RuntimeDiscoverableAttrs -disable-availability-checking %S/Inputs/runtime_attrs.swift
// RUN: %target-swift-frontend %use_no_opaque_pointers -primary-file %s -emit-ir -I %t -swift-version 5 -enable-experimental-feature RuntimeDiscoverableAttrs -disable-availability-checking | %IRGenFileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -I %t -swift-version 5 -enable-experimental-feature RuntimeDiscoverableAttrs -disable-availability-checking

// REQUIRES: asserts
// REQUIRES: OS=macosx
// REQUIRES: concurrency

// : @"$s28runtime_attributes_on_distributed_actors9TestActorC15asyncExternallyyyKcvpfaAA17FlagForAsyncFuncsHF"
// : @"$s28runtime_attributes_on_distributed_actors9TestActorC11doSomethingyyYaKcvpfaAA17FlagForAsyncFuncsHF"
// : @"$s28runtime_attributes_on_distributed_actors9TestActorC11doSomethingyySiYacvpfaAA17FlagForAsyncFuncsHF"
// : @"$s28runtime_attributes_on_distributed_actors9TestActorC11doSomething_1xySi_SaySiGztYacvpfaAA17FlagForAsyncFuncsHF"

// : @"$s40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVHa" = internal constant
// SAME: i32 5   4
// SAME: %swift.accessible_function* @"s40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVAA9TestActorC15asyncExternallyyyKFfaHF"
// SAME: %swift.accessible_function* @"$s40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyyYaKFfaHF"
// SAME: %swift.accessible_function* @"$s40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyySiYaFfaHF"
// SAME: %swift.accessible_function* @"$s40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVAA9TestActorC11doSomething_1xySi_SaySiGtYaFfaHF"

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

  // CHECK-LABEL: define hidden swiftcc void @"$s40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVAA9TestActorC15asyncExternallyyyKFfa"(%T40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs distributed func asyncExternally() throws {}

  // CHECK-LABEL: define hidden swiftcc void @"$s40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyyYaKFfa"(%T40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs distributed func doSomething() async throws {}

  // CHECK-LABEL: define hidden swiftcc void @"$s40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyySiYaFfa"(%T40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs nonisolated func doSomething(_: Int) async {}

  // CHECK-LABEL: define hidden swiftcc void @"$s40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVAA9TestActorC11doSomething_1xySi_SaySiGtYaFfa"(%T40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T40runtime_attributes_on_distributed_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs distributed func doSomething(_: Int, x: [Int]) async {}
}
