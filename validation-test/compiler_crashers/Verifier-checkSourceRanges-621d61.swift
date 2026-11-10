// {"kind":"typecheck","original":"bd10060a","signature":"(anonymous namespace)::Verifier::checkSourceRanges(swift::SourceRange, swift::ASTWalker::ParentTy, llvm::function_ref<void ()>)","signatureNext":"Verifier::walkToStmtPost"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a {
  b {
  "\( active type { (of value[ ))"
  }
