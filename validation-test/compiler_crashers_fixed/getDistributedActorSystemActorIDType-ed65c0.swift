// RUN: not %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
distributed actor DefaultDistributedActorSystem: LocalTestingDistributedActorSystem {
}
