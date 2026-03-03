// {"kind":"typecheck","signature":"swift::rewriting::RewriteContext::getProtocolComponentRec(swift::ProtocolDecl const*, llvm::SmallVectorImpl<swift::ProtocolDecl const*>&)","signatureAssert":"Assertion failed: (found != Dependencies.end()), function getProtocolComponentRec"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a : b protocol b : c protocol d : a class c < e : d
