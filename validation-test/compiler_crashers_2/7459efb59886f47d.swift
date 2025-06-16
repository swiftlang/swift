// {"signature":"swift::DerivedConformance::deriveDistributedActor(swift::ValueDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem distributed actor a{c, b
