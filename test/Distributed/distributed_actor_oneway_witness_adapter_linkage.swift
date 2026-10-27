// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift

// Without whole module optimization two files of a module can both need the
// adapter of the same function, and each one emits it, so it has to be a
// definition that the linker can merge
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t -module-name main -primary-file %t/src/a.swift %t/src/b.swift %t/src/c.swift | %FileCheck %s --check-prefix=A
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t -module-name main -primary-file %t/src/b.swift %t/src/a.swift %t/src/c.swift | %FileCheck %s --check-prefix=B

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// A: define linkonce_odr hidden {{.*}}@"$s4main6ActorAC19$onewayWitness$pingyyKFTE"
// B: define linkonce_odr hidden {{.*}}@"$s4main6ActorAC19$onewayWitness$pingyyKFTE"

//--- a.swift
@_spi(OnewaySend) import Distributed
import FakeDistributedActorSystems

public distributed actor ActorA {
  public typealias ActorSystem = FakeRoundtripActorSystem

  public distributed func ping() {}
}

protocol OneP: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func ping()
}

extension ActorA: OneP {}

//--- b.swift
@_spi(OnewaySend) import Distributed
import FakeDistributedActorSystems

protocol TwoP: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func ping()
}

extension ActorA: TwoP {}

//--- c.swift
