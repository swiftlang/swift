// {"kind":"typecheck","original":"240d55bd","signature":"swift::checkConformance(swift::Type, swift::ProtocolDecl*, bool)","signatureNext":"conformsToDifferentiable"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@transpose(of: a, wrt: 0) func b(c: <#type#>) -> d
