// {"kind":"typecheck","signature":"swift::constraints::TupleContextualFailure::TupleContextualFailure(swift::constraints::Solution const&, swift::ContextualTypePurpose, swift::Type, swift::Type, llvm::ArrayRef<unsigned int>, swift::constraints::ConstraintLocator*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a< each b >->(Int, repeat each b)Float= a(
