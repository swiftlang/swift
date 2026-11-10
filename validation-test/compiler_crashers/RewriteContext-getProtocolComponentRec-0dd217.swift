// {"kind":"typecheck","signature":"swift::rewriting::RewriteContext::getProtocolComponentRec(swift::ProtocolDecl const*, llvm::SmallVectorImpl<swift::ProtocolDecl const*>&)","signatureAssert":"Assertion failed: (found != Dependencies.end()), function getProtocolComponentRec","signatureNext":"RewriteContext::getProtocolComponentImpl"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a : b protocol b : c protocol d : a class c < e : d
