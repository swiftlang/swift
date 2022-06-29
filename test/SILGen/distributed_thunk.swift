// RUN:  %target-swift-emit-silgen %s -enable-experimental-distributed -disable-availability-checking | %FileCheck %s --dump-input=always
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

distributed actor DA {
  typealias ActorSystem = LocalTestingDistributedActorSystem
}

extension DA {
  // CHECK-LABEL: sil hidden [thunk] [distributed] [ref_adhoc_requirement_witness "$s11Distributed29LocalTestingInvocationDecoderC18decodeNextArgumentxyKSeRzSERzlF"] [ossa] @$s17distributed_thunk2DAC1fyyYaKFTE : $@convention(method) @async (@guaranteed DA) -> @error Error {
  // CHECK: function_ref @swift_distributed_actor_is_remote

  // Call the actor function
  // CHECK: function_ref @$s17distributed_thunk2DAC1fyyF : $@convention(method) (@guaranteed DA) -> ()

  distributed func f() { }
}

protocol ServerProto {
  func doSomething() async throws
}

extension DA: ServerProto {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s17distributed_thunk2DACAA11ServerProtoA2aDP11doSomethingyyYaKFTW : $@convention(witness_method: ServerProto) @async (@in_guaranteed DA) -> @error Error
  // CHECK: function_ref @$s17distributed_thunk2DAC11doSomethingyyF
  // CHECK: return
  distributed func doSomething() { }
}

distributed actor DA2: ServerProto {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s17distributed_thunk3DA2CAA11ServerProtoA2aDP11doSomethingyyYaKFTW : $@convention(witness_method: ServerProto) @async (@in_guaranteed DA2) -> @error Error
  // CHECK-NOT: return
  // CHECK: class_method %1 : $DA2, #DA2.doSomething : (isolated DA2) -> () async -> (), $@convention(method) @async (@guaranteed DA2) -> ()
  distributed func doSomething() async { }
}

distributed actor DA3: ServerProto {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s17distributed_thunk3DA3CAA11ServerProtoA2aDP11doSomethingyyYaKFTW
  // CHECK-NOT: return
  // CHECK: class_method %1 : $DA3, #DA3.doSomething : (isolated DA3) -> () async throws -> (), $@convention(method) @async (@guaranteed DA3) -> @error Error
  distributed func doSomething() async throws { }
}

distributed actor DA4: ServerProto {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s17distributed_thunk3DA4CAA11ServerProtoA2aDP11doSomethingyyYaKFTW
  // CHECK: hop_to_executor
  // CHECK-NOT: return
  // CHECK: class_method %3 : $DA4, #DA4.doSomething : (isolated DA4) -> () throws -> (), $@convention(method) (@guaranteed DA4) -> @error Error
  distributed func doSomething() throws { }
}
