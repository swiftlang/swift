// {"kind":"typecheck","original":"92c14a71","signature":"swift::getDistributedActorSystemActorIDType(swift::NominalTypeDecl*)","signatureAssert":"Assertion failed: (!system->isDistributedActor()), function getDistributedActorSystemActorIDType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
distributed actor DefaultDistributedActorSystem: LocalTestingDistributedActorSystem {
}
