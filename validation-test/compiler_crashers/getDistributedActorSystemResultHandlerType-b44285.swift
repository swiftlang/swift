// {"kind":"emit-silgen","signature":"swift::getDistributedActorSystemResultHandlerType(swift::NominalTypeDecl*)","signatureAssert":"Assertion failed: (!system->isDistributedActor()), function getDistributedActorSystemResultHandlerType","signatureNext":"DerivedConformance::canDeriveDistributedActorSystem"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
import Distributed
distributed actor a: DistributedActorSystem {
}
