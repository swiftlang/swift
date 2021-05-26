// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -enable-experimental-distributed | %IRGenFileCheck %s
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

// Type descriptor.
// CHECK-LABEL: @"$s23distributed_actor_class7MyActorC0B7Address12_Distributed0eF0VvpWvd"

@available(SwiftStdlib 5.5, *)
public distributed actor MyActor {
    // nothing
}
