// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::diagnoseAmbiguityWithFixes(llvm::SmallVectorImpl<swift::constraints::Solution>&)","signatureAssert":"Assertion failed: (fn), function diagnoseAmbiguity"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  lazy b : Int -> Int = {
    b(0.0
    1
  }
  b : Int -> Int
