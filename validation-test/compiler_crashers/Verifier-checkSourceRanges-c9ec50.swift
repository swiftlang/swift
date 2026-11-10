// {"kind":"typecheck","signature":"(anonymous namespace)::Verifier::checkSourceRanges(swift::SourceRange, swift::ASTWalker::ParentTy, llvm::function_ref<void ()>)","signatureNext":"Verifier::walkToPatternPost"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func == {
  switch {
  case.a {
