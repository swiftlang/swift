// {"kind":"typecheck","original":"e51afbca","signature":"swift::constraints::ConstraintSystem::diagnoseAmbiguityWithFixes(llvm::SmallVectorImpl<swift::constraints::Solution>&)","signatureAssert":"Assertion failed: (!empty()), function front"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a throw withThrowingTaskGroup(of : a) {
b in
