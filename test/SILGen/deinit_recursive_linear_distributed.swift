// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Distributed/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -target %target-swift-5.7-abi-triple -I %t %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed

// Same as deinit_recursive_linear.swift, but for a distributed actor,
// which needs special handling because the remote references deinit.
// A node may be a remote proxy, which has no storage for user-declared stored
// properties, so the loop must check for remote before reading `next` out of it

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Node {
  var elem: [Int64] = []
  var next: Node?
}

// CHECK-LABEL: sil hidden [ossa] @$s35deinit_recursive_linear_distributed4NodeCfd : $@convention(method) (@guaranteed Node) -> @owned Builtin.NativeObject {
// CHECK:   [[ITER:%.*]] = alloc_stack $Optional<Node>
// CHECK:   br [[LOOPBB:bb[0-9]+]]

// CHECK: [[LOOPBB]]:
// CHECK:   [[ITER_COPY:%.*]] = load [copy] [[ITER]] : $*Optional<Node>
// CHECK:   switch_enum [[ITER_COPY]] : $Optional<Node>, case #Optional.some!enumelt: [[IS_SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[IS_NONE_BB:bb[0-9]+]]

// Check for remote before anything reads `next` out of the node
// CHECK: [[IS_SOME_BB]]([[NODE:%.*]] : @owned $Node):
// CHECK:   destroy_value [[NODE]] : $Node
// CHECK:   [[ITER_BORROW:%.*]] = load_borrow [[ITER]] : $*Optional<Node>
// CHECK:   [[ITER_UNWRAPPED:%.*]] = unchecked_enum_data [[ITER_BORROW]] : $Optional<Node>, #Optional.some!enumelt
// CHECK:   [[ITER_ANYOBJECT:%.*]] = init_existential_ref [[ITER_UNWRAPPED]] : $Node : $Node, $AnyObject
// CHECK:   [[IS_REMOTE_FN:%.*]] = function_ref @swift_distributed_actor_is_remote
// CHECK:   [[IS_REMOTE:%.*]] = apply [[IS_REMOTE_FN]]([[ITER_ANYOBJECT]])
// CHECK:   [[IS_REMOTE_I1:%.*]] = struct_extract [[IS_REMOTE]] : $Bool, #Bool._value
// CHECK:   end_borrow [[ITER_BORROW]] : $Optional<Node>
// CHECK:   cond_br [[IS_REMOTE_I1]], [[REMOTE_BB:bb[0-9]+]], [[LOCAL_BB:bb[0-9]+]]

// CHECK: [[IS_UNIQUE_BB:bb[0-9]+]]:
// CHECK:   [[ITER_BORROW:%.*]] = load_borrow [[ITER]] : $*Optional<Node>
// CHECK:   [[ITER_UNWRAPPED:%.*]] = unchecked_enum_data [[ITER_BORROW]] : $Optional<Node>, #Optional.some!enumelt
// CHECK:   [[NEXT_ADDR:%.*]] = ref_element_addr [[ITER_UNWRAPPED]] : $Node, #Node.next
// CHECK:   [[NEXT_ADDR_ACCESS:%.*]] = begin_access [read] [static] [no_nested_conflict] [[NEXT_ADDR]] : $*Optional<Node>
// CHECK:   [[NEXT_COPY:%.*]] = load [copy] [[NEXT_ADDR_ACCESS]] : $*Optional<Node>
// CHECK:   end_access [[NEXT_ADDR_ACCESS]] : $*Optional<Node>
// CHECK:   end_borrow [[ITER_BORROW]] : $Optional<Node>
// CHECK:   store [[NEXT_COPY]] to [assign] [[ITER]] : $*Optional<Node>
// CHECK:   br [[LOOPBB]]

// CHECK: [[NOT_UNIQUE_BB:bb[0-9]+]]:
// CHECK:   br [[CLEAN_BB:bb[0-9]+]]

// CHECK: [[IS_NONE_BB]]:
// CHECK:   br [[CLEAN_BB]]

// CHECK: [[CLEAN_BB]]:
// CHECK:   destroy_addr [[ITER]] : $*Optional<Node>
// CHECK:   dealloc_stack [[ITER]] : $*Optional<Node>
// CHECK:   return

// CHECK: [[LOCAL_BB]]:
// CHECK:   [[IS_UNIQUE:%.*]] = is_unique [[ITER]] : $*Optional<Node>
// CHECK:   cond_br [[IS_UNIQUE]], [[IS_UNIQUE_BB]], [[NOT_UNIQUE_BB]]

// A remote proxy ends the chain, it is released by the normal cleanup
// CHECK: [[REMOTE_BB]]:
// CHECK-NEXT: br [[CLEAN_BB]]
// CHECK: } // end sil function '$s35deinit_recursive_linear_distributed4NodeCfd'
