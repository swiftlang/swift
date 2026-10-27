// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Distributed/Inputs/FakeDistributedActorSystems.swift

// A public '@oneway' distributed func has two public distributed
// thunks: the synchronous one, which a 'send' calls, and the 'async' await
// thunk, which any other call calls. A public '@oneway' requirement
// has a requirement thunk, a method descriptor, and with library evolution a
// dispatch thunk, for each of them
// RUN: %target-swift-frontend %s -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t -emit-ir -o /dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all
// RUN: %target-swift-frontend %s -enable-library-evolution -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t -emit-ir -o /dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all
// RUN: %target-swift-frontend %s -enable-library-evolution -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t -emit-ir -o /dev/null -parse-as-library -module-name test -emit-tbd -emit-tbd-path %t/test.tbd -tbd-install_name test
// RUN: %FileCheck %s < %t/test.tbd
// RUN: %FileCheck %s --check-prefix=NEG < %t/test.tbd

import Distributed
import FakeDistributedActorSystems

public protocol Teller: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  @oneway distributed func tell(_ n: Int)
}

public distributed actor Worker: Teller {
  public typealias ActorSystem = FakeRoundtripActorSystem

  @oneway public distributed func tell(_ n: Int) {}
}

// The synchronous thunk, the await thunk and its async function pointer
// CHECK-DAG: "_$s4test6WorkerC4tellyySiKFTE"
// CHECK-DAG: "_$s4test6WorkerC4tellyySiYaKFTE"
// CHECK-DAG: "_$s4test6WorkerC4tellyySiYaKFTETu"

// The dispatch thunks and method descriptors of both requirement thunks
// CHECK-DAG: "_$s4test6TellerP4tellyySiKFTjTE"
// CHECK-DAG: "_$s4test6TellerP4tellyySiKFTqTE"
// CHECK-DAG: "_$s4test6TellerP4tellyySiYaKFTjTE"
// CHECK-DAG: "_$s4test6TellerP4tellyySiYaKFTjTETu"
// CHECK-DAG: "_$s4test6TellerP4tellyySiYaKFTqTE"

// The synchronous thunk has no async function pointer
// NEG-NOT: "_$s4test6WorkerC4tellyySiKFTETu"
