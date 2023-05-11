// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path=%t/RAD.swiftmodule -module-name=RAD -enable-experimental-feature RuntimeDiscoverableAttrs -disable-availability-checking %S/Inputs/runtime_attrs.swift
// RUN: %target-swift-frontend %use_no_opaque_pointers -primary-file %s -emit-ir -I %t -swift-version 5 -enable-experimental-feature RuntimeDiscoverableAttrs -disable-availability-checking | %IRGenFileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -I %t -swift-version 5 -enable-experimental-feature RuntimeDiscoverableAttrs -disable-availability-checking

// REQUIRES: asserts
// REQUIRES: OS=macosx
// REQUIRES: concurrency

// CHECK: @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC15asyncExternallyyyKFfaHF"
// CHECK: @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyyYaKFfaHF"
// CHECK: @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyySiYaFfaHF"
// CHECK: @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC11doSomething_1xySi_SaySiGztYaFfaHF"
// CHECK: @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA06globalG2FnSaySSGyYaFfaHF"

// CHECK: @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVHa" = internal constant
// CHECK-SAME: i32 5
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC15asyncExternallyyyKFfaHF"
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyyYaKFfaHF"
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyySiYaFfaHF"
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC11doSomething_1xySi_SaySiGztYaFfaHF"
// CHECK-SAME: %swift.accessible_function* @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA06globalG2FnSaySSGyYaFfaHF"

@runtimeMetadata
struct FlagForAsyncFuncs {
  init<Act>(attachedTo: (Act) async throws -> Void) {}
  init<Act>(attachedTo: (Act, Int, inout [Int]) async throws -> Void) {}
  init<Act>(attachedTo: (Act, Int) async -> Void) {}
  init(attachedTo: () async -> [String]) {}
}

actor TestActor {
  // CHECK-LABEL: define hidden swiftcc void @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC15asyncExternallyyyKFfa"(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs func asyncExternally() throws {
  }

  // CHECK-LABEL: define hidden swiftcc void @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyyYaKFfa"(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs func doSomething() async throws {
  }

  // CHECK-LABEL: define hidden swiftcc void @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC11doSomethingyySiYaFfa"(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs nonisolated func doSomething(_: Int) async {
  }

  // CHECK-LABEL: define hidden swiftcc void @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA9TestActorC11doSomething_1xySi_SaySiGztYaFfa"(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg) %0)
  @FlagForAsyncFuncs func doSomething(_: Int, x: inout [Int]) async {
  }
}

// CHECK-LABEL: define hidden swiftcc void @"$s28runtime_attributes_on_actors17FlagForAsyncFuncsVAA06globalG2FnSaySSGyYaFfa"(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg* noalias nocapture sret(%T28runtime_attributes_on_actors17FlagForAsyncFuncsVSg) %0)
@FlagForAsyncFuncs
func globalAsyncFn() async -> [String] {
  return []
}
