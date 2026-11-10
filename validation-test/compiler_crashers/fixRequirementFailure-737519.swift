// {"kind":"typecheck","signature":"fixRequirementFailure(swift::constraints::ConstraintSystem&, swift::Type, swift::Type, swift::ASTNode, llvm::ArrayRef<swift::constraints::ConstraintLocator::PathElement>)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::simplifySubclassOfConstraint"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
class a func b< each c : a >(repeat each c) b( d repeat {
