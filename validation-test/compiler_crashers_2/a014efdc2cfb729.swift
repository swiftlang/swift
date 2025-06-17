// {"signature":"swift::getDistributedActorSystemSerializationType(swift::NominalTypeDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed typealias a = LocalTestingDistributedActorSystem protocol a{distributed actor b:a
