// {"signature":"swift::constraints::ConstraintSystem::setCapturedExpansions(swift::AnyFunctionRef, llvm::SmallVector<swift::PackExpansionExpr*, 1u>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
{
  repeat a {
