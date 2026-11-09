// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main -o /dev/null

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// A 'send' on a distributed actor which is isolated to the caller, and
// therefore known to be local, calls the function itself rather than its
// distributed thunk, just like a 'send' of a method of a plain actor on the
// caller's own actor: a synchronous function is enqueued on the actor with
// '_enqueueOnewayIsolatedAny', an 'async' one runs in a new task. A 'send' on
// an actor which is not isolated to the caller still calls the synchronous
// distributed thunk of a '@oneway' function. See
// distributed_actor_oneway_send_isolated_self.swift for the diagnostics

@_spi(OnewaySend) import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Worker {
  @oneway distributed func tell(_ n: Int) {}
  @oneway distributed func tellAsync(_ n: Int) async {}
  distributed func plain(_ n: Int) {}

  // CHECK-LABEL: sil hidden [ossa] @$s4main6WorkerC13isolatedSendsyyF :
  // CHECK-NOT: TE :
  // CHECK-NOT: remoteCallOneway
  // CHECK: [[TELL:%.*]] = function_ref @$s4main6WorkerC13isolatedSendsyyFyycfU_ :
  // CHECK: [[TELL_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[TELL]](
  // CHECK: [[ENQUEUE:%.*]] = function_ref @$ss25_enqueueOnewayIsolatedAnyyyyyYAcF :
  // CHECK: apply [[ENQUEUE]]([[TELL_CLOSURE]])
  // CHECK-NOT: TE :
  // CHECK: [[TELL_ASYNC:%.*]] = function_ref @$s4main6WorkerC13isolatedSendsyyFyyYaKXOfU0_ :
  // CHECK: [[TELL_ASYNC_CLOSURE:%.*]] = partial_apply [called_once] [[TELL_ASYNC]](
  // CHECK: builtin "createAsyncTask"({{.*}}, [[TELL_ASYNC_CLOSURE]])
  // CHECK-NOT: TE :
  // CHECK: [[PLAIN:%.*]] = function_ref @$s4main6WorkerC13isolatedSendsyyFyycfU1_ :
  // CHECK: [[PLAIN_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[PLAIN]](
  // CHECK: [[ENQUEUE:%.*]] = function_ref @$ss25_enqueueOnewayIsolatedAnyyyyyYAcF :
  // CHECK: apply [[ENQUEUE]]([[PLAIN_CLOSURE]])
  // CHECK-NOT: TE :
  // CHECK-NOT: remoteCallOneway
  // CHECK: } // end sil function '$s4main6WorkerC13isolatedSendsyyF'

  // The enqueued closures call the functions themselves
  // CHECK-LABEL: sil private [ossa] @$s4main6WorkerC13isolatedSendsyyFyycfU_ :
  // CHECK: function_ref @$s4main6WorkerC4tellyySiF :
  // CHECK: } // end sil function '$s4main6WorkerC13isolatedSendsyyFyycfU_'
  // CHECK-LABEL: sil private [ossa] @$s4main6WorkerC13isolatedSendsyyFyyYaKXOfU0_ :
  // CHECK: function_ref @$s4main6WorkerC9tellAsyncyySiYaF :
  // CHECK: } // end sil function '$s4main6WorkerC13isolatedSendsyyFyyYaKXOfU0_'
  // CHECK-LABEL: sil private [ossa] @$s4main6WorkerC13isolatedSendsyyFyycfU1_ :
  // CHECK: function_ref @$s4main6WorkerC5plainyySiF :
  // CHECK: } // end sil function '$s4main6WorkerC13isolatedSendsyyFyycfU1_'
  func isolatedSends() {
    send self.tell(1)
    send self.tellAsync(2)
    send self.plain(3)
  }

  // A nonisolated 'self' may be remote: the synchronous distributed thunk
  // CHECK-LABEL: sil hidden [ossa] @$s4main6WorkerC5kappayyKF :
  // CHECK: [[THUNK:%.*]] = function_ref @$s4main6WorkerC4tellyySiKFTE :
  // CHECK: try_apply [[THUNK]](
  // CHECK: } // end sil function '$s4main6WorkerC5kappayyKF'
  nonisolated func kappa() throws {
    try send self.tell(4)
  }

  // The isolation of a closure is not known when the 'send' is lowered, so
  // even a 'send' in a closure which is isolated to the actor conservatively
  // calls the synchronous distributed thunk
  // CHECK-LABEL: sil private [ossa] @$s4main6WorkerC9inClosureyyKFyyKcfU_ :
  // CHECK: [[THUNK:%.*]] = function_ref @$s4main6WorkerC4tellyySiKFTE :
  // CHECK: try_apply [[THUNK]](
  // CHECK: } // end sil function '$s4main6WorkerC9inClosureyyKFyyKcfU_'
  func inClosure() throws {
    let c = { try send self.tell(5) }
    try c()
  }
}

// An 'isolated' parameter is known to be local too
// CHECK-LABEL: sil hidden [ossa] @$s4main1f1dyAA6WorkerCYi_tF :
// CHECK-NOT: TE :
// CHECK: [[TELL:%.*]] = function_ref @$s4main1f1dyAA6WorkerCYi_tFyycfU_ :
// CHECK: [[TELL_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[TELL]](
// CHECK: [[ENQUEUE:%.*]] = function_ref @$ss25_enqueueOnewayIsolatedAnyyyyyYAcF :
// CHECK: apply [[ENQUEUE]]([[TELL_CLOSURE]])
// CHECK-NOT: TE :
// CHECK: } // end sil function '$s4main1f1dyAA6WorkerCYi_tF'
// CHECK-LABEL: sil private [ossa] @$s4main1f1dyAA6WorkerCYi_tFyycfU_ :
// CHECK: function_ref @$s4main6WorkerC4tellyySiF :
// CHECK: } // end sil function '$s4main1f1dyAA6WorkerCYi_tFyycfU_'
func f(d: isolated Worker) {
  send d.tell(6)
}

// A non-isolated reference may be remote: the synchronous distributed thunk
// CHECK-LABEL: sil hidden [ossa] @$s4main1g1dyAA6WorkerC_tKF :
// CHECK: [[THUNK:%.*]] = function_ref @$s4main6WorkerC4tellyySiKFTE :
// CHECK: try_apply [[THUNK]](
// CHECK: } // end sil function '$s4main1g1dyAA6WorkerC_tKF'
func g(d: Worker) throws {
  try send d.tell(7)
}

protocol Teller: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func tell(_ n: Int)
}

// Through a protocol requirement, on an 'isolated' generic actor: the
// requirement itself rather than the witness of its distributed thunk
// CHECK-LABEL: sil hidden [ossa] @$s4main1hyyxYiAA6TellerRzlF :
// CHECK: [[TELL:%.*]] = function_ref @$s4main1hyyxYiAA6TellerRzlFyycfU_ :
// CHECK: [[TELL_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[TELL]]<T>(
// CHECK: [[ENQUEUE:%.*]] = function_ref @$ss25_enqueueOnewayIsolatedAnyyyyyYAcF :
// CHECK: apply [[ENQUEUE]]([[TELL_CLOSURE]])
// CHECK: } // end sil function '$s4main1hyyxYiAA6TellerRzlF'
// CHECK-LABEL: sil private [ossa] @$s4main1hyyxYiAA6TellerRzlFyycfU_ :
// CHECK: witness_method $T, #Teller.tell!distributed :
// CHECK: } // end sil function '$s4main1hyyxYiAA6TellerRzlFyycfU_'
func h<T: Teller>(_ t: isolated T) {
  send t.tell(8)
}
