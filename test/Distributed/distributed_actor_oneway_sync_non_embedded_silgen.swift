// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main | %FileCheck %s --check-prefix=THUNK
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main | %FileCheck %s --check-prefix=IR

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// A '@oneway' distributed func, 'async' or not, has a synchronous, throwing
// distributed thunk in every mode, and 'try send' calls it directly, without
// a task at the call site. The thunk hands a remote call to
// 'remoteCallOneway' and sends a local call to the func itself. The
// distributed target accessor, which is 'async', dispatches to the async
// recipient thunk instead, but its accessible function record keeps the name
// of the synchronous thunk, which is the remote call target identifier

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

// The distributed thunk is synchronous and throwing
// CHECK-LABEL: sil hidden [thunk] [distributed]{{.*}} @$s4main7GreeterC5greetyySiKFTE : $@convention(method) (@sil_sending Int, @guaranteed Greeter) -> @error any Error {
// CHECK: function_ref @$s27FakeDistributedActorSystems0a9RoundtripC6SystemC16remoteCallOneway2on6target10invocation
// CHECK: [[OPERATION_FN:%.*]] = function_ref @$s4main7GreeterC5greetyySiKFyyYaKXOfU_ :
// CHECK: builtin "createAsyncTask"
// CHECK: } // end sil function '$s4main7GreeterC5greetyySiKFTE'
// CHECK-LABEL: sil private [ossa] @$s4main7GreeterC5greetyySiKFyyYaKXOfU_ :
// CHECK: function_ref @$s4main7GreeterC5greetyySiF :
// CHECK-LABEL: sil hidden [distributed_proxy_adapter_thunk] [ossa] @$s4main7GreeterC30$distributedProxyAdapter$greetyySiYaKF : $@convention(method) @async (@sil_sending Int, @guaranteed Greeter) -> @error any Error {
// CHECK: function_ref @$s4main7GreeterC5greetyySiF :

// The thunk of an 'async' '@oneway' func is synchronous too
// CHECK-LABEL: sil hidden [thunk] [distributed]{{.*}} @$s4main7GreeterC10greetLateryySiKFTE : $@convention(method) (@sil_sending Int, @guaranteed Greeter) -> @error any Error {
// CHECK-LABEL: sil hidden [distributed_proxy_adapter_thunk] [ossa] @$s4main7GreeterC35$distributedProxyAdapter$greetLateryySiYaKF : $@convention(method) @async (@sil_sending Int, @guaranteed Greeter) -> @error any Error {
distributed actor Greeter {
  @oneway distributed func greet(_ n: Int) {}
  @oneway distributed func greetLater(_ n: Int) async {}
}

// The local branch of the thunk sends the call to the func itself. There
// the initial serial executor of the task is derived at runtime via an
// 'isRemote' check: if remote, '.none' (no local executor to enqueue on); if
// local, the actor's own serial executor, same as the plain actor-instance
// case, merged at the continuation block. In the thunk the actor is always
// local
// THUNK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main13RemoteGreeterC5greetyyKFTE : $@convention(method) (@guaranteed RemoteGreeter) -> @error any Error {
// THUNK: cond_br {{%.*}}, {{bb[0-9]+}}, [[LOCAL_BRANCH:bb[0-9]+]]
// THUNK: [[LOCAL_BRANCH]]:
// THUNK: [[RECEIVER:%.*]] = copy_value %0
// THUNK: [[BASE:%.*]] = move_value [lexical] [var_decl] [[RECEIVER]]
// THUNK: [[BASE_BORROW:%.*]] = begin_borrow [[BASE]]
// THUNK: [[ACTOR:%.*]] = copy_value [[BASE_BORROW]]
// THUNK: [[BORROW:%.*]] = begin_borrow [[ACTOR]]
// THUNK: [[ANYOBJ:%.*]] = init_existential_ref [[BORROW]]
// THUNK: [[IS_REMOTE_FN:%.*]] = function_ref @swift_distributed_actor_is_remote
// THUNK: [[IS_REMOTE:%.*]] = apply [[IS_REMOTE_FN]]([[ANYOBJ]])
// THUNK: [[IS_REMOTE_I1:%.*]] = struct_extract [[IS_REMOTE]]
// THUNK: cond_br [[IS_REMOTE_I1]], [[REMOTE_BB:bb[0-9]+]], [[LOCAL_BB:bb[0-9]+]]
//
// THUNK: [[LOCAL_BB]]:
// THUNK: [[LOCAL_BORROW:%.*]] = begin_borrow [[ACTOR]]
// THUNK: [[LOCAL_EXEC:%.*]] = builtin "buildDefaultActorExecutorRef"<RemoteGreeter>([[LOCAL_BORROW]] : $RemoteGreeter) : $Builtin.Executor
// THUNK: [[LOCAL_SOME_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[LOCAL_EXEC]] : $Builtin.Executor
// THUNK: br [[CONT_BB:bb[0-9]+]]([[LOCAL_SOME_EXEC]] : $Optional<Builtin.Executor>)
//
// THUNK: [[REMOTE_BB]]:
// THUNK: [[REMOTE_NONE_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
// THUNK: br [[CONT_BB]]([[REMOTE_NONE_EXEC]] : $Optional<Builtin.Executor>)
//
// THUNK: [[CONT_BB]]([[EXEC:%.*]] : $Optional<Builtin.Executor>):
// THUNK: [[OPERATION_FN:%.*]] = function_ref @$s4main13RemoteGreeterC5greetyyKFyyYaKXOfU_ :
// THUNK: [[OPERATION:%.*]] = partial_apply [called_once] [[OPERATION_FN]]
// THUNK: [[FLAGS_INT:%.*]] = integer_literal $Builtin.Int64, 54272
// THUNK: [[FLAGS:%.*]] = struct $Int ([[FLAGS_INT]]
// THUNK: builtin "createAsyncTask"([[FLAGS]] : $Int, [[EXEC]] : $Optional<Builtin.Executor>, {{.*}}, [[OPERATION]] :
// THUNK: } // end sil function '$s4main13RemoteGreeterC5greetyyKFTE'

// The task's operation calls the func itself on the known-to-be-local actor,
// not the thunk
// THUNK-LABEL: sil private [ossa] @$s4main13RemoteGreeterC5greetyyKFyyYaKXOfU_ : $@convention(thin) @async (@sil_sending @guaranteed RemoteGreeter) -> @error any Error {
// THUNK-NOT: TE :
// THUNK: function_ref @$s4main13RemoteGreeterC5greetyyF :
// THUNK: } // end sil function '$s4main13RemoteGreeterC5greetyyKFyyYaKXOfU_'
distributed actor RemoteGreeter {
  @oneway distributed func greet() {}
}

// A 'send' from a synchronous function calls the synchronous thunks
// directly, without creating a task
// CHECK-LABEL: sil hidden [ossa] @$s4main15sendDistributedyyAA7GreeterCKF : $@convention(thin) (@guaranteed Greeter) -> @error any Error {
// CHECK-NOT: createAsyncTask
// CHECK: function_ref @$s4main7GreeterC5greetyySiKFTE : $@convention(method) (@sil_sending Int, @guaranteed Greeter) -> @error any Error
// CHECK-NOT: createAsyncTask
// CHECK: function_ref @$s4main7GreeterC10greetLateryySiKFTE : $@convention(method) (@sil_sending Int, @guaranteed Greeter) -> @error any Error
// CHECK-NOT: createAsyncTask
// CHECK: } // end sil function '$s4main15sendDistributedyyAA7GreeterCKF'
func sendDistributed(_ g: Greeter) throws {
  try send g.greet(3)
  try send g.greetLater(4)
}

// The accessible function records carry the names of the synchronous thunks
// IR-DAG: @"$s4main7GreeterC5greetyySiKFTEHF" = private constant %swift.accessible_function { {{.*}}@".str.30.$s4main7GreeterC5greetyySiKFTE"{{.*}}@"$s4main7GreeterC5greetyySiKFTETFTu"
// IR-DAG: @"$s4main7GreeterC10greetLateryySiKFTEHF" = private constant %swift.accessible_function { {{.*}}@".str.36.$s4main7GreeterC10greetLateryySiKFTE"{{.*}}@"$s4main7GreeterC10greetLateryySiKFTETFTu"

// The synchronous thunk is a plain function
// IR-DAG: define hidden swiftcc void @"$s4main7GreeterC5greetyySiKFTE"(i64 %0, ptr swiftself %1, ptr noalias swifterror captures(none) dereferenceable(8) %2)

// The accessor dispatches to the async recipient thunk
// IR-LABEL: define linkonce_odr hidden swifttailcc void @"$s4main7GreeterC5greetyySiKFTETF"(
// IR-NOT: call swiftcc void @"$s4main7GreeterC5greetyySiKFTE"(
// IR: musttail call swifttailcc void @"$s4main7GreeterC30$distributedProxyAdapter$greetyySiYaKF"(

// The call site calls the synchronous thunks and creates no task
// IR-LABEL: define hidden swiftcc void @"$s4main15sendDistributedyyAA7GreeterCKF"(
// IR-NOT: swift_task_create
// IR: call swiftcc void @"$s4main7GreeterC5greetyySiKFTE"(i64 3,
// IR-NOT: swift_task_create
// IR: call swiftcc void @"$s4main7GreeterC10greetLateryySiKFTE"(i64 4,
// IR-NOT: swift_task_create
// IR: ret void
