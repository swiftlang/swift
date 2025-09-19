// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::setCapturedExpansions(swift::AnyFunctionRef, llvm::SmallVector<swift::PackExpansionExpr*, 1u>)","signatureAssert":"Assertion failed: (CapturedExpansions.count(func) == 0 && \"Cannot reset captured expansions\"), function setCapturedExpansions"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  repeat a {
