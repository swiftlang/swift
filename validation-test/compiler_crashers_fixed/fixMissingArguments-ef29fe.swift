// {"kind":"typecheck","languageMode":6,"signature":"fixMissingArguments(swift::constraints::ConstraintSystem&, swift::ASTNode, llvm::SmallVectorImpl<swift::AnyFunctionType::Param>&, llvm::ArrayRef<swift::AnyFunctionType::Param>, unsigned int, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]"}
// RUN: not %target-swift-frontend -typecheck -swift-version 6 %s
func a((Int, Int, Int)) a > {
  b, c in
