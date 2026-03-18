// {"kind":"typecheck","original":"bd10060a","signature":"(anonymous namespace)::Verifier::checkSourceRanges(swift::SourceRange, swift::ASTWalker::ParentTy, llvm::function_ref<void ()>)","signatureNext":"Verifier::walkToStmtPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a {
  b {
  "\( active type { (of value[ ))"
  }
