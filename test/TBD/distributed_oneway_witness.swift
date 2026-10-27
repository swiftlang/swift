// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Distributed/Inputs/FakeDistributedActorSystems.swift

// A public '@oneway' requirement witnessed by a plain function is witnessed by
// the 'oneway witness adapter' of that function, which is a thunk of the module
// RUN: %target-swift-frontend %s -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t -emit-ir -o /dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all
// RUN: %target-swift-frontend %s -enable-library-evolution -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t -emit-ir -o /dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all

@_spi(OnewaySend) import Distributed
import FakeDistributedActorSystems

@_spi(OnewaySend)
public protocol Pinger: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func ping(_ n: Int)
}

@_spi(OnewaySend)
public distributed actor RelaxedPinger: Pinger {
  public typealias ActorSystem = FakeRoundtripActorSystem

  public distributed func ping(_ n: Int) {}
}

// A public function that witnesses the requirement of an internal protocol:
// the adapter is shared, so it is not part of the API of the module
protocol InternalPinger: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func ping(_ n: Int)
}

public distributed actor InternalRelaxedPinger: InternalPinger {
  public typealias ActorSystem = FakeRoundtripActorSystem

  public distributed func ping(_ n: Int) {}
}
