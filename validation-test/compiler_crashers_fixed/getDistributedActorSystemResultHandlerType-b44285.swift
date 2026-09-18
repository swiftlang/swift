// RUN: not %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
import Distributed
distributed actor a: DistributedActorSystem {
}
