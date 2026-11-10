// {"kind":"typecheck","signature":"checkDistributedTargetResultType(swift::ValueDecl*, swift::Type, bool)","signatureNext":"checkDistributedActorProperty"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed protocol a:DistributedActor{distributed actor:
