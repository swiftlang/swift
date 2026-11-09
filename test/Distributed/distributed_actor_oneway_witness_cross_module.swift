// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Lib.swiftmodule -module-name Lib -parse-as-library -target %target-swift-6.2-abi-triple -disable-availability-checking -I %t %s -DLIB -enable-experimental-feature SendCalls

// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -o /dev/null -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s -module-name main -validate-tbd-against-ir=all

// The same with libraries and clients built with library evolution
// RUN: %empty-directory(%t/resilient)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/resilient/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -enable-library-evolution -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/resilient/Lib.swiftmodule -module-name Lib -enable-library-evolution -parse-as-library -target %target-swift-6.2-abi-triple -disable-availability-checking -I %t/resilient %s -DLIB -enable-experimental-feature SendCalls
// RUN: %target-swift-frontend -emit-ir -o /dev/null -enable-library-evolution -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t/resilient %s -module-name main -validate-tbd-against-ir=all

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// A '@oneway' requirement of a protocol is witnessed by a plain function of a
// distributed actor of another module. The module of the conformance emits the
// 'oneway witness adapter' of that function, which makes remote calls with the
// identifier of the function and calls it by 'send' for a local call

@_spi(OnewaySend) import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

#if LIB

public distributed actor Worker {
  public distributed func work(_ n: Int) {}
}

#else

import Lib

protocol Workable: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func work(_ n: Int)
}

extension Worker: Workable {}

// The adapter is a synchronous, throwing thunk with shared linkage, because the
// module of any conformance emits it. A remote call is made by 'remoteCallOneway'
// with the identifier of the thunk of the function (which is asynchronous), a
// local call is a 'send' of the call of the function
// CHECK-LABEL: sil shared {{(\[serialized\] )?}}[thunk] [distributed] {{.*}}@$s3Lib6WorkerC19$onewayWitness$workyySiKFTE : $@convention(method) (@sil_sending Int, @guaranteed Worker) -> @error any Error
// CHECK: string_literal utf8 "$s3Lib6WorkerC4workyySiYaKFTE"
// CHECK: _markOnewayRemoteCall
// CHECK: remoteCallOneway
// CHECK: closure #1 in Worker.$onewayWitness$work
// CHECK: } // end sil function '$s3Lib6WorkerC19$onewayWitness$workyySiKFTE'

// The witness of the requirement thunk calls the adapter
// CHECK-LABEL: sil private [transparent] [distributed_thunk] [ossa] @$s3Lib6WorkerC4main8WorkableA2dEP4workyySiKFTWTE
// CHECK: function_ref @$s3Lib6WorkerC19$onewayWitness$workyySiKFTE

#endif
