// {"signature":"fixMissingArguments(swift::constraints::ConstraintSystem&, swift::ASTNode, llvm::SmallVectorImpl<swift::AnyFunctionType::Param>&, llvm::ArrayRef<swift::AnyFunctionType::Param>, unsigned int, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-frontend -typecheck -swift-version 6 %s
func a((Int, Int, Int)) a > {
  b, c in
