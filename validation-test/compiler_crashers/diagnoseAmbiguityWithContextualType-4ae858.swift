// {"kind":"typecheck","signature":"diagnoseAmbiguityWithContextualType(swift::constraints::ConstraintSystem&, swift::constraints::SolutionDiff&, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>, llvm::ArrayRef<swift::constraints::Solution>)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::diagnoseAmbiguityWithFixes"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a func b< c >(c = a(
    func a
