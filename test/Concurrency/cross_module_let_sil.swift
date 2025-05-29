// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -target %target-swift-5.1-abi-triple
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -verify -module-name test -I %t -target %target-swift-5.1-abi-triple -strict-concurrency=complete %s | %FileCheck %s --implicit-check-not=hop_to_executor --enable-var-scope
// REQUIRES: concurrency

import OtherActors

// CHECK-LABEL: sil hidden [ossa] @$s4test6check1ySi11OtherActors0C11ModuleActorCYaF : $@convention(thin) @async (@guaranteed OtherModuleActor) -> Int {
// CHECK:     bb0([[SELF:%[0-9]+]] : @guaranteed $OtherModuleActor):
// CHECK:       [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// CHECK-NEXT:  hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// CHECK:       [[REF:%[0-9]+]] = ref_element_addr [[SELF]] : $OtherModuleActor, #OtherModuleActor.a
// CHECK:       hop_to_executor [[SELF]] : $OtherModuleActor
// CHECK-NEXT:  load [trivial] [[REF]]
// CHECK-NEXT:  hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// CHECK: } // end sil function '$s4test6check1ySi11OtherActors0C11ModuleActorCYaF'
func check1(_ actor: OtherModuleActor) async -> Int {
  return await actor.a
}

func check2(_ actor: isolated OtherModuleActor) -> Int {
  return actor.a
}

// CHECK-LABEL: sil hidden [ossa] @$s4test6check3ySi11OtherActors0C11ModuleActorCYaF : $@convention(thin) @async (@guaranteed OtherModuleActor) -> Int {
// CHECK:     bb0([[SELF:%[0-9]+]] : @guaranteed $OtherModuleActor):
// CHECK:       [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// CHECK-NEXT:  hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
func check3(_ actor: OtherModuleActor) async -> Int {
  return actor.b
}

// CHECK-LABEL: sil hidden [ossa] @$s4test6check4y11OtherActors17SomeSendableClassCSgAC0C11ModuleActorCSgYaF : $@convention(thin) @async (@guaranteed Optional<OtherModuleActor>) -> @owned Optional<SomeSendableClass> {
// CHECK:  bb0({{%[0-9]+}} : @guaranteed $Optional<OtherModuleActor>):
// CHECK:       [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// CHECK-NEXT:  hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// CHECK:       switch_enum {{%[0-9]+}} : $Optional<OtherModuleActor>, case #Optional.some!enumelt: [[SOME:bb[0-9]+]], case #Optional.none!enumelt: {{bb[0-9]+}}

// CHECK:  [[SOME]]({{%[0-9]+}} : @owned $OtherModuleActor):
// CHECK:       [[REF:%[0-9]+]] = ref_element_addr {{%[0-9]+}} : $OtherModuleActor, #OtherModuleActor.d
// CHECK:       hop_to_executor {{%[0-9]+}} : $OtherModuleActor
// CHECK-NEXT:  load [copy] [[REF]]
// CHECK:       hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// CHECK: } // end sil function '$s4test6check4y11OtherActors17SomeSendableClassCSgAC0C11ModuleActorCSgYaF'
func check4(_ actor: OtherModuleActor?) async -> SomeSendableClass? {
  return await actor?.d
}
