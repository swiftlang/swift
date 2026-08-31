// RUN: not %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
distributed actor a<d: DistributedActorSystem> {
  typealias ActorSystem = d
  distributed func b() -> c
}
