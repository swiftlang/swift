// {"kind":"typecheck","signature":"(anonymous namespace)::Verifier::checkSourceRanges(swift::SourceRange, swift::ASTWalker::ParentTy, llvm::function_ref<void ()>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func == {
  switch {
  case.a {
