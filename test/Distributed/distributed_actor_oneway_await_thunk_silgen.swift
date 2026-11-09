// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main -verify | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main | %FileCheck %s --check-prefix=IR --implicit-check-not='tellyySiKFTEHF' --implicit-check-not='tellyySiKFTETF' --implicit-check-not='.$s4main6WorkerC4tellyySiKFTE"' --implicit-check-not='distributedProxyAdapter'
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main -validate-tbd-against-ir=all

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// A '@oneway' distributed func has two distributed thunks:
//
// - the synchronous thunk, which a 'send' calls, and which hands a remote
//   call to 'remoteCallOneway' as a 'oneway' remote call
// - the 'async' await thunk, which any other call calls, e.g. an 'await'.
//   It is the thunk of a regular function returning 'Void': it waits for a
//   local call, and makes a remote call through 'remoteCallVoid', which is
//   not marked as a 'oneway' remote call
//
// Both use the remote call target identifier of the await thunk, which is the
// identifier that the func would have if it was not '@oneway', and the name of
// its one accessible function record, so the recipient runs sent and awaited
// calls the same way. Only 'isOnewayRemoteCall' tells them apart. Both mark the
// target as the target of a '@oneway' func ('isOnewayTarget'). The thunks of a
// plain func, and the oneway witness adapter of a plain witness of a '@oneway'
// requirement, do not

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

protocol Teller: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  @oneway distributed func tell(_ n: Int)
}

distributed actor Worker: Teller {
  @oneway distributed func tell(_ n: Int) {}
}

// A non-'@oneway' witness of a '@oneway' requirement
distributed actor PlainWorker: Teller {
  distributed func tell(_ n: Int) {}
}

// ==== -----------------------------------------------------------------------
// MARK: The thunks

// The synchronous thunk: a remote call is a 'oneway' remote call
// CHECK-LABEL: sil hidden [thunk] [distributed]{{.*}} @$s4main6WorkerC4tellyySiKFTE : $@convention(method) (@sil_sending Int, @guaranteed Worker) -> @error any Error {
// CHECK: string_literal utf8 "$s4main6WorkerC4tellyySiYaKFTE"
// CHECK: // function_ref RemoteCallTarget._markOnewayRemoteCall()
// CHECK: // function_ref RemoteCallTarget._markOnewayTarget()
// CHECK-NEXT: function_ref @$s11Distributed16RemoteCallTargetV{{.*}}markOneway{{.*}} :
// CHECK: // function_ref FakeRoundtripActorSystem.remoteCallOneway<A>(on:target:invocation:)
// CHECK: } // end sil function '$s4main6WorkerC4tellyySiKFTE'

// The await thunk: 'async', with the same identifier, a remote call which is
// not a 'oneway' remote call, and a local call that is waited for
// CHECK-LABEL: sil hidden [thunk] [distributed]{{.*}} @$s4main6WorkerC4tellyySiYaKFTE : $@convention(method) @async (@sil_sending Int, @guaranteed Worker) -> @error any Error {
// CHECK-NOT: _markOnewayRemoteCall
// CHECK-NOT: remoteCallOneway
// CHECK: string_literal utf8 "$s4main6WorkerC4tellyySiYaKFTE"
// CHECK-NOT: _markOnewayRemoteCall
// CHECK-NOT: remoteCallOneway
// CHECK: // function_ref RemoteCallTarget._markOnewayTarget()
// CHECK-NOT: _markOnewayRemoteCall
// CHECK-NOT: remoteCallOneway
// CHECK: // function_ref FakeRoundtripActorSystem.remoteCallVoid<A, B>(on:target:invocation:throwing:)
// CHECK-NOT: _markOnewayRemoteCall
// CHECK-NOT: remoteCallOneway
// CHECK: [[LOCAL_FN:%.*]] = function_ref @$s4main6WorkerC4tellyySiF :
// CHECK: apply [[LOCAL_FN]]
// CHECK-NOT: _enqueueOneway
// CHECK-NOT: createAsyncTask
// CHECK: } // end sil function '$s4main6WorkerC4tellyySiYaKFTE'

// The thunks of the plain witness do not mark the target as the target of a
// '@oneway' func, neither does its oneway witness adapter, which makes a
// 'oneway' remote call of it with the identifier of its regular thunk
// CHECK-LABEL: sil hidden [thunk] [distributed]{{.*}} @$s4main11PlainWorkerC4tellyySiYaKFTE :
// CHECK-NOT: _markOnewayTarget
// CHECK: } // end sil function '$s4main11PlainWorkerC4tellyySiYaKFTE'
// CHECK-LABEL: sil shared [thunk] [distributed]{{.*}} @$s4main11PlainWorkerC19$onewayWitness$tellyySiKFTE :
// CHECK-NOT: _markOnewayTarget
// CHECK: string_literal utf8 "$s4main11PlainWorkerC4tellyySiYaKFTE"
// CHECK-NOT: _markOnewayTarget
// CHECK: // function_ref RemoteCallTarget._markOnewayRemoteCall()
// CHECK-NOT: _markOnewayTarget
// CHECK: // function_ref FakeRoundtripActorSystem.remoteCallOneway<A>(on:target:invocation:)
// CHECK-NOT: _markOnewayTarget
// CHECK: } // end sil function '$s4main11PlainWorkerC19$onewayWitness$tellyySiKFTE'

// The witness thunk of the await requirement thunk calls the regular thunk of
// a plain witness
// CHECK-LABEL: sil private [transparent] [distributed_thunk] [ossa] @$s4main11PlainWorkerCAA6TellerA2aDP4tellyySiYaKFTWTE :
// CHECK: function_ref @$s4main11PlainWorkerC4tellyySiYaKFTE :

// ==== -----------------------------------------------------------------------
// MARK: Call sites

// CHECK-LABEL: sil hidden [ossa] @$s4main10sendWorkeryyAA0C0CYaKF :
// CHECK: function_ref @$s4main6WorkerC4tellyySiKFTE :
// CHECK-NOT: function_ref @$s4main6WorkerC4tellyySiYaKFTE
// CHECK: } // end sil function '$s4main10sendWorkeryyAA0C0CYaKF'
func sendWorker(_ w: Worker) async throws {
  try send w.tell(1)
}

// CHECK-LABEL: sil hidden [ossa] @$s4main11awaitWorkeryyAA0C0CYaKF :
// CHECK: function_ref @$s4main6WorkerC4tellyySiYaKFTE :
// CHECK-NOT: function_ref @$s4main6WorkerC4tellyySiKFTE
// CHECK: } // end sil function '$s4main11awaitWorkeryyAA0C0CYaKF'
func awaitWorker(_ w: Worker) async throws {
  try await w.tell(1) // expected-warning{{'@oneway' distributed instance method 'tell' is not intended to be awaited on, use 'send' instead}}
}

// Through the requirement: 'send' calls the synchronous requirement thunk,
// 'await' the await requirement thunk
// CHECK-LABEL: sil hidden [ossa] @$s4main10sendTelleryyxYaKAA0C0RzlF :
// CHECK: witness_method $T, #Teller.tell!distributed_thunk : <Self where Self : Teller> (Self) -> (sending Int) throws -> () :
// CHECK: } // end sil function '$s4main10sendTelleryyxYaKAA0C0RzlF'
func sendTeller<T: Teller>(_ t: T) async throws {
  try send t.tell(1)
}

// CHECK-LABEL: sil hidden [ossa] @$s4main11awaitTelleryyxYaKAA0C0RzlF :
// CHECK: witness_method $T, #Teller.tell!distributed_thunk : <Self where Self : Teller> (Self) -> (sending Int) async throws -> () :
// CHECK: } // end sil function '$s4main11awaitTelleryyxYaKAA0C0RzlF'
func awaitTeller<T: Teller>(_ t: T) async throws {
  try await t.tell(1) // expected-warning{{'@oneway' distributed instance method 'tell' is not intended to be awaited on, use 'send' instead}}
}

// ==== -----------------------------------------------------------------------
// MARK: Witness tables


// A '@oneway' witness: its synchronous thunk and its await thunk
// CHECK-LABEL: sil_witness_table hidden Worker: Teller module main {
// CHECK:      method #Teller.tell!distributed_thunk: <Self where Self : Teller> (Self) -> (sending Int) throws -> () : @$s4main6WorkerCAA6TellerA2aDP4tellyySiKFTWTE
// CHECK-NEXT: method #Teller.tell!distributed_thunk: <Self where Self : Teller> (Self) -> (sending Int) async throws -> () : @$s4main6WorkerCAA6TellerA2aDP4tellyySiYaKFTWTE

// A plain witness: its oneway witness adapter and its regular thunk
// CHECK-LABEL: sil_witness_table hidden PlainWorker: Teller module main {
// CHECK:      method #Teller.tell!distributed_thunk: <Self where Self : Teller> (Self) -> (sending Int) throws -> () : @$s4main11PlainWorkerCAA6TellerA2aDP4tellyySiKFTWTE
// CHECK-NEXT: method #Teller.tell!distributed_thunk: <Self where Self : Teller> (Self) -> (sending Int) async throws -> () : @$s4main11PlainWorkerCAA6TellerA2aDP4tellyySiYaKFTWTE

// ==== -----------------------------------------------------------------------
// MARK: IR

// One accessible function record per func, named after the await thunk, like
// the record of a plain func, whose accessor calls the await thunk directly.
// The synchronous thunk has no record and no accessor, its name is no remote
// call target identifier, and there is no resolvable proxy adapter thunk (see
// the '--implicit-check-not's)
// IR-DAG: @"$s4main6WorkerC4tellyySiYaKFTEHF" = private constant %swift.accessible_function { {{.*}}@".str.{{[0-9]+}}.$s4main6WorkerC4tellyySiYaKFTE"{{.*}}@"$s4main6WorkerC4tellyySiYaKFTETFTu"
// IR-DAG: @"$s4main11PlainWorkerC4tellyySiYaKFTEHF" = private constant
// IR-DAG: define {{.*}}swifttailcc void @"$s4main6WorkerC4tellyySiYaKFTETF"(
// IR-DAG: define hidden swift{{(tail)?}}cc void @"$s4main6WorkerC4tellyySiYaKFTE"(
// IR-DAG: define hidden swift{{(tail)?}}cc void @"$s4main6WorkerC4tellyySiKFTE"(
