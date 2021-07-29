// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -enable-experimental-distributed | %IRGenFileCheck %s
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

// Type descriptor.
// CHECK-LABEL: @"$s17distributed_actor7MyActorC0B9Transport12_Distributed0dE0_pvpWvd"

@available(SwiftStdlib 5.5, *)
public distributed actor MyActor {
    // nothing
}
