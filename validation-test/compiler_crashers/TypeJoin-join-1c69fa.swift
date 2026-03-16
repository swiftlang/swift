// {"kind":"typecheck","original":"3736af0c","signature":"(anonymous namespace)::TypeJoin::join(swift::CanType, swift::CanType)","signatureAssert":"Assertion failed: (cast<ProtocolCompositionType>(First)->getInverses().empty() && \"FIXME: move-only generics\"), function visitProtocolCompositionType","signatureNext":"TypeJoin::visitMetatypeType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
< a & ~Escapable & ~Copyable
