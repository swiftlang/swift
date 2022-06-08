// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -disable-availability-checking | %IRGenFileCheck %s
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

// Type descriptor.
// CHECK-LABEL: @"$s17distributed_actor7MyActorC2id11Distributed012LocalTestingD2IDVvpWvd"
@available(SwiftStdlib 5.6, *)
public distributed actor MyActor {
  public typealias ActorSystem = LocalTestingDistributedActorSystem
  // nothing
}
