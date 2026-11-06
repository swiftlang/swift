// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::diagnoseAmbiguityWithFixes(llvm::SmallVectorImpl<swift::constraints::Solution>&)","signatureAssert":"Assertion failed: (fn), function diagnoseAmbiguity","signatureNext":"ConstraintSystem::salvage"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  lazy b : Int -> Int = {
    b(0.0
    1
  }
  b : Int -> Int
