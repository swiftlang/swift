// {"kind":"typecheck","original":"b1b2ade5","signature":"(anonymous namespace)::TypeJoin::visitProtocolCompositionType(swift::CanType)","signatureAssert":"Assertion failed: (cast<ProtocolCompositionType>(second)->getInverses().empty() && \"FIXME: move-only generics\"), function visitProtocolCompositionType","signatureNext":"TypeJoin::join"}
// RUN: not --crash %target-swift-frontend -typecheck %s
Copyable & ~Escapable
func Copyable()
