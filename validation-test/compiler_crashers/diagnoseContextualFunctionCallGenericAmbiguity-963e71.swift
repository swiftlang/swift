// {"kind":"typecheck","original":"e51afbca","signature":"diagnoseContextualFunctionCallGenericAmbiguity(swift::constraints::ConstraintSystem&, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>)","signatureAssert":"Assertion failed: (!empty()), function front","signatureNext":"ConstraintSystem::diagnoseAmbiguityWithFixes"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a throw withThrowingTaskGroup(of : a) {
b in
