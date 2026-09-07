// RUN: not %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
@attached(extension conformances: DistributedActor) macro a()
@a distributed actor b {
  distributed init()
}
