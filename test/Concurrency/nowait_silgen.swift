// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Distributed/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -I %t %s -module-name main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

actor Greeter {
  func greet() {}
}

@MainActor
final class GlobalGreeter {
  func greet() {}
}

distributed actor RemoteGreeter {
  distributed func greet() {}
}

// 'nowait' lowers to a discarding 'createAsyncTask': flags carry
// CopyTaskLocals|EnqueueJob|IsDiscardingTask (== 21504), no future fragment.
// On a plain actor-instance target, the actor's own serial executor is
// derived via 'buildDefaultActorExecutorRef' and passed as the task's
// initial serial executor, so successive 'nowait' calls to the same actor
// enqueue in program order (FIFO)
// CHECK-LABEL: sil hidden [ossa] @$s4main4testyyAA7GreeterCKF
func test(_ g: Greeter) throws {
  // CHECK: [[FLAGS_INT:%.*]] = integer_literal $Builtin.Int64, 21504
  // CHECK: [[FLAGS:%.*]] = struct $Int ([[FLAGS_INT]]
  // CHECK: [[ACTOR:%.*]] = copy_value %0
  // CHECK: [[BORROW:%.*]] = begin_borrow [[ACTOR]]
  // CHECK: [[EXEC:%.*]] = builtin "buildDefaultActorExecutorRef"<Greeter>([[BORROW]] : $Greeter) : $Builtin.Executor
  // CHECK: [[SOME_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[EXEC]] : $Builtin.Executor
  // CHECK: builtin "createAsyncTask"([[FLAGS]] : $Int, [[SOME_EXEC]] : $Optional<Builtin.Executor>,
  nowait g.greet()
}

// On a global-actor-isolated target, the global actor's own executor is
// derived (the main actor has a dedicated builtin, since it isn't a
// default-actor executor) and set as the initial serial executor too
// CHECK-LABEL: sil hidden [ossa] @$s4main10testGlobalyyAA0C7GreeterCKF
func testGlobal(_ g: GlobalGreeter) throws {
  // CHECK: [[EXEC:%.*]] = builtin "buildMainActorExecutorRef"() : $Builtin.Executor
  // CHECK: [[SOME_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[EXEC]] : $Builtin.Executor
  // CHECK: builtin "createAsyncTask"({{.*}} : $Int, [[SOME_EXEC]] : $Optional<Builtin.Executor>,
  nowait g.greet()
}

// On a distributed-actor target, the initial serial executor is derived at
// runtime via an 'isRemote' check: if remote, '.none' (no local executor to
// enqueue on - the distributed thunk performs the 'remoteCall' itself); if
// local, the actor's own serial executor, same as the plain actor-instance
// case, merged at the continuation block
// CHECK-LABEL: sil hidden [ossa] @$s4main15testDistributedyyAA13RemoteGreeterCKF
func testDistributed(_ g: RemoteGreeter) throws {
  // CHECK: [[FLAGS_INT:%.*]] = integer_literal $Builtin.Int64, 21504
  // CHECK: [[FLAGS:%.*]] = struct $Int ([[FLAGS_INT]]
  // CHECK: [[ACTOR:%.*]] = copy_value %0
  // CHECK: [[BORROW:%.*]] = begin_borrow [[ACTOR]]
  // CHECK: [[ANYOBJ:%.*]] = init_existential_ref [[BORROW]]
  // CHECK: [[IS_REMOTE_FN:%.*]] = function_ref @swift_distributed_actor_is_remote
  // CHECK: [[IS_REMOTE:%.*]] = apply [[IS_REMOTE_FN]]([[ANYOBJ]])
  // CHECK: [[IS_REMOTE_I1:%.*]] = struct_extract [[IS_REMOTE]]
  // CHECK: cond_br [[IS_REMOTE_I1]], [[REMOTE_BB:bb[0-9]+]], [[LOCAL_BB:bb[0-9]+]]
  //
  // CHECK: [[CONT_BB:bb[0-9]+]]([[EXEC:%.*]] : $Optional<Builtin.Executor>):
  // CHECK: builtin "createAsyncTask"([[FLAGS]] : $Int, [[EXEC]] : $Optional<Builtin.Executor>,
  //
  // CHECK: [[LOCAL_BB]]:
  // CHECK: [[LOCAL_BORROW:%.*]] = begin_borrow [[ACTOR]]
  // CHECK: [[LOCAL_EXEC:%.*]] = builtin "buildDefaultActorExecutorRef"<RemoteGreeter>([[LOCAL_BORROW]] : $RemoteGreeter) : $Builtin.Executor
  // CHECK: [[LOCAL_SOME_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[LOCAL_EXEC]] : $Builtin.Executor
  // CHECK: br [[CONT_BB]]([[LOCAL_SOME_EXEC]] : $Optional<Builtin.Executor>)
  //
  // CHECK: [[REMOTE_BB]]:
  // CHECK: [[REMOTE_NONE_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
  // CHECK: br [[CONT_BB]]([[REMOTE_NONE_EXEC]] : $Optional<Builtin.Executor>)
  nowait g.greet()
}
