// {"kind":"typecheck","signature":"checkDistributedTargetResultType(swift::ValueDecl*, swift::Type, bool)","signatureNext":"checkDistributedActorProperty"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed protocol a:DistributedActor{distributed actor:
