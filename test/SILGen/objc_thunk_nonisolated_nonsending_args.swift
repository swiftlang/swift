// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -module-name objc_thunk_nonisolated_nonsending_args -target %target-swift-5.1-abi-triple %s | %FileCheck %s
// RUN: %target-swift-emit-sil(mock-sdk: %clang-importer-sdk) -module-name objc_thunk_nonisolated_nonsending_args -target %target-swift-5.1-abi-triple %s -o /dev/null
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -module-name objc_thunk_nonisolated_nonsending_args -target %target-swift-5.1-abi-triple -enable-upcoming-feature NonisolatedNonsendingByDefault %s | %FileCheck --check-prefixes=CHECK,NNBD %s
// RUN: %target-swift-emit-sil(mock-sdk: %clang-importer-sdk) -module-name objc_thunk_nonisolated_nonsending_args -target %target-swift-5.1-abi-triple -enable-upcoming-feature NonisolatedNonsendingByDefault %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// rdar://185745777
//
// The native entry point of an @objc nonisolated(nonsending) async method has
// an implicit leading isolated parameter that the ObjC thunk does not. Each
// bridged argument must use the ownership and indirectness convention of its
// own native parameter, not that of the parameter before it.

import Foundation

final class Payload: NSObject {}

@objc final class Host: NSObject {
  // A consuming argument is forwarded into the @owned slot; self is borrowed.
  // CHECK-LABEL: sil shared [thunk] [ossa] @$s38objc_thunk_nonisolated_nonsending_args4HostC4takeyyAA7PayloadCnYaFyyYacfU_To : $@convention(thin) @Sendable @async (Payload, @convention(block) () -> (), Host) -> () {
  // CHECK: bb0([[X:%.*]] : @unowned $Payload, [[BLOCK:%.*]] : @unowned $@convention(block) () -> (), [[SELF:%.*]] : @unowned $Host):
  // CHECK:   [[ACTOR:%.*]] = unchecked_value_cast {{%.*}} to $Builtin.ImplicitActor
  // CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK-NOT: begin_borrow [[X_COPY]]
  // CHECK:   [[SELF_BORROW:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[FN:%.*]] = function_ref @$s38objc_thunk_nonisolated_nonsending_args4HostC4takeyyAA7PayloadCnYaF : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @owned Payload, @guaranteed Host) -> ()
  // CHECK:   apply [[FN]]([[ACTOR]], [[X_COPY]], [[SELF_BORROW]])
  // CHECK-NEXT: end_borrow [[SELF_BORROW]]
  // CHECK-NEXT: destroy_value [[SELF_COPY]]
  // CHECK-NOT: destroy_value [[X_COPY]]
  // CHECK: } // end sil function '$s38objc_thunk_nonisolated_nonsending_args4HostC4takeyyAA7PayloadCnYaFyyYacfU_To'
  @objc nonisolated(nonsending) func take(_ x: consuming Payload) async {}

  // Direct URL followed by a guaranteed Payload: both arguments and self are
  // borrowed or passed directly, and nothing is loaded.
  // CHECK-LABEL: sil shared [thunk] [ossa] @$s38objc_thunk_nonisolated_nonsending_args4HostC5take2yy10Foundation3URLV_AA7PayloadCtYaFyyYacfU_To : $@convention(thin) @Sendable @async (NSURL, Payload, @convention(block) () -> (), Host) -> () {
  // CHECK: bb0({{%.*}} : @unowned $NSURL, [[P:%.*]] : @unowned $Payload, {{%.*}} : @unowned $@convention(block) () -> (), [[SELF:%.*]] : @unowned $Host):
  // CHECK:   [[ACTOR:%.*]] = unchecked_value_cast {{%.*}} to $Builtin.ImplicitActor
  // CHECK:   [[P_COPY:%.*]] = copy_value [[P]]
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[URL:%.*]] = apply {{%.*}}({{%.*}}, {{%.*}}) : $@convention(method) (@guaranteed Optional<NSURL>, @thin URL.Type) -> URL
  // CHECK-NOT: load
  // CHECK:   [[P_BORROW:%.*]] = begin_borrow [[P_COPY]]
  // CHECK:   [[SELF_BORROW:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[FN:%.*]] = function_ref @$s38objc_thunk_nonisolated_nonsending_args4HostC5take2yy10Foundation3URLV_AA7PayloadCtYaF : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, URL, @guaranteed Payload, @guaranteed Host) -> ()
  // CHECK:   apply [[FN]]([[ACTOR]], [[URL]], [[P_BORROW]], [[SELF_BORROW]])
  // CHECK-NEXT: end_borrow [[SELF_BORROW]]
  // CHECK-NEXT: end_borrow [[P_BORROW]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK-NEXT: destroy_value [[P_COPY]]
  // CHECK: } // end sil function '$s38objc_thunk_nonisolated_nonsending_args4HostC5take2yy10Foundation3URLV_AA7PayloadCtYaFyyYacfU_To'
  @objc nonisolated(nonsending) func take2(_ u: URL, _ p: Payload) async {}

  // An address-only argument is passed by address without a load, and the
  // consuming argument after it is forwarded.
  // CHECK-LABEL: sil shared [thunk] [ossa] @$s38objc_thunk_nonisolated_nonsending_args4HostC5take3yyyp_AA7PayloadCntYaFyyYacfU_To : $@convention(thin) @Sendable @async (AnyObject, Payload, @convention(block) () -> (), Host) -> () {
  // CHECK: bb0({{%.*}} : @unowned $AnyObject, [[P:%.*]] : @unowned $Payload, {{%.*}} : @unowned $@convention(block) () -> (), [[SELF:%.*]] : @unowned $Host):
  // CHECK:   [[ACTOR:%.*]] = unchecked_value_cast {{%.*}} to $Builtin.ImplicitActor
  // CHECK:   [[P_COPY:%.*]] = copy_value [[P]]
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[ANY:%.*]] = alloc_stack $Any
  // CHECK:   apply {{%.*}}([[ANY]], {{%.*}}) : $@convention(thin) (@guaranteed Optional<AnyObject>) -> @out Any
  // CHECK-NOT: load
  // CHECK-NOT: begin_borrow [[P_COPY]]
  // CHECK:   [[SELF_BORROW:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[FN:%.*]] = function_ref @$s38objc_thunk_nonisolated_nonsending_args4HostC5take3yyyp_AA7PayloadCntYaF : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed Any, @owned Payload, @guaranteed Host) -> ()
  // CHECK:   apply [[FN]]([[ACTOR]], [[ANY]], [[P_COPY]], [[SELF_BORROW]])
  // CHECK-NEXT: end_borrow [[SELF_BORROW]]
  // CHECK-NEXT: destroy_addr [[ANY]]
  // CHECK-NEXT: dealloc_stack [[ANY]]
  // CHECK-NOT: destroy_value [[P_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK-NOT: destroy_value [[P_COPY]]
  // CHECK: } // end sil function '$s38objc_thunk_nonisolated_nonsending_args4HostC5take3yyyp_AA7PayloadCntYaFyyYacfU_To'
  @objc nonisolated(nonsending) func take3(_ a: Any, _ p: consuming Payload) async {}

  // CHECK-LABEL: sil shared [thunk] [ossa] @$s38objc_thunk_nonisolated_nonsending_args4HostC10takeStaticyyyp_AA7PayloadCntYaFZyyYacfU_To : $@convention(thin) @Sendable @async (AnyObject, Payload, @convention(block) () -> (), @objc_metatype Host.Type) -> () {
  // CHECK: bb0({{%.*}} : @unowned $AnyObject, [[P:%.*]] : @unowned $Payload, {{%.*}} : @unowned $@convention(block) () -> (), [[SELF:%.*]] : $@objc_metatype Host.Type):
  // CHECK:   [[ACTOR:%.*]] = unchecked_value_cast {{%.*}} to $Builtin.ImplicitActor
  // CHECK:   [[P_COPY:%.*]] = copy_value [[P]]
  // CHECK:   [[ANY:%.*]] = alloc_stack $Any
  // CHECK-NOT: load
  // CHECK-NOT: begin_borrow [[P_COPY]]
  // CHECK:   [[META:%.*]] = objc_to_thick_metatype [[SELF]] to $@thick Host.Type
  // CHECK:   [[FN:%.*]] = function_ref @$s38objc_thunk_nonisolated_nonsending_args4HostC10takeStaticyyyp_AA7PayloadCntYaFZ : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed Any, @owned Payload, @thick Host.Type) -> ()
  // CHECK:   apply [[FN]]([[ACTOR]], [[ANY]], [[P_COPY]], [[META]])
  // CHECK-NEXT: destroy_addr [[ANY]]
  // CHECK-NOT: destroy_value [[P_COPY]]
  // CHECK: } // end sil function '$s38objc_thunk_nonisolated_nonsending_args4HostC10takeStaticyyyp_AA7PayloadCntYaFZyyYacfU_To'
  @objc nonisolated(nonsending) static func takeStatic(_ a: Any, _ p: consuming Payload) async {}
}

// With NonisolatedNonsendingByDefault, a plain @objc async method is
// nonisolated(nonsending) too, so it gets the same implicit leading parameter.
@objc class ImplicitHost: NSObject {
  // NNBD-LABEL: sil shared [thunk] [ossa] @$s38objc_thunk_nonisolated_nonsending_args12ImplicitHostC4takeyyyp_AA7PayloadCntYaFyyYacfU_To : $@convention(thin) @Sendable @async (AnyObject, Payload, @convention(block) () -> (), ImplicitHost) -> () {
  // NNBD: bb0({{%.*}} : @unowned $AnyObject, [[P:%.*]] : @unowned $Payload, {{%.*}} : @unowned $@convention(block) () -> (), [[SELF:%.*]] : @unowned $ImplicitHost):
  // NNBD:   [[ACTOR:%.*]] = unchecked_value_cast {{%.*}} to $Builtin.ImplicitActor
  // NNBD:   [[P_COPY:%.*]] = copy_value [[P]]
  // NNBD:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // NNBD:   [[ANY:%.*]] = alloc_stack $Any
  // NNBD-NOT: load
  // NNBD-NOT: begin_borrow [[P_COPY]]
  // NNBD:   [[SELF_BORROW:%.*]] = begin_borrow [[SELF_COPY]]
  // NNBD:   [[FN:%.*]] = function_ref @$s38objc_thunk_nonisolated_nonsending_args12ImplicitHostC4takeyyyp_AA7PayloadCntYaF : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed Any, @owned Payload, @guaranteed ImplicitHost) -> ()
  // NNBD:   apply [[FN]]([[ACTOR]], [[ANY]], [[P_COPY]], [[SELF_BORROW]])
  // NNBD:   destroy_value [[SELF_COPY]]
  // NNBD-NOT: destroy_value [[P_COPY]]
  // NNBD: } // end sil function '$s38objc_thunk_nonisolated_nonsending_args12ImplicitHostC4takeyyyp_AA7PayloadCntYaFyyYacfU_To'
  @objc func take(_ a: Any, _ p: consuming Payload) async {}
}
