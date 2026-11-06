// {"kind":"typecheck","signature":"checkDistributedTargetResultType(swift::ValueDecl*, swift::Type, bool)","signatureNext":"checkDistributedActorProperty"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed protocol a:DistributedActor{distributed actor:
