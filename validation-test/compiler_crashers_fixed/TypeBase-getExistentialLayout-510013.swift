// RUN: not %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
distributed actor a<ActorSystem: DistributedActorSystem> {
  distributed func b(c: <#type#>)
}
