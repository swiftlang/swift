// {"kind":"typecheck","signature":"swift::getDistributedActorSystemSerializationType(swift::NominalTypeDecl*)","signatureAssert":"Assertion failed: (!system->isDistributedActor()), function getDistributedActorSystemSerializationType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed typealias a = LocalTestingDistributedActorSystem protocol a{distributed actor b:a
