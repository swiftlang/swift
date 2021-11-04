// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -enable-experimental-distributed | %IRGenFileCheck %s
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

// Type descriptor.
// CHECK-LABEL: @"$s17distributed_actor7MyActorC2id12_Distributed03AnyD8IdentityVvpWvd"

@available(SwiftStdlib 5.6, *)
public distributed actor MyActor {
    // nothing
}
