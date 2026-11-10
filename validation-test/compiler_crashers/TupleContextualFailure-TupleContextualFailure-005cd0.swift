// {"kind":"typecheck","signature":"swift::constraints::TupleContextualFailure::TupleContextualFailure(swift::constraints::Solution const&, swift::ContextualTypePurpose, swift::Type, swift::Type, llvm::ArrayRef<unsigned int>, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (getFromType()->is<TupleType>() && getToType()->is<TupleType>()), function TupleContextualFailure","signatureNext":"AllowTupleTypeMismatch::coalesceAndDiagnose"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a< each b >->(Int, repeat each b)Float= a(
