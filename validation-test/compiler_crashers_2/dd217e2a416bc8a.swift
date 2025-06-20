// {"signature":"swift::rewriting::RewriteContext::getProtocolComponentRec(swift::ProtocolDecl const*, llvm::SmallVectorImpl<swift::ProtocolDecl const*>&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a : b protocol b : c protocol d : a class c < e : d
