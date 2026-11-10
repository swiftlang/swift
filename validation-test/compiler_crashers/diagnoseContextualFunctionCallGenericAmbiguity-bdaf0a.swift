// {"kind":"typecheck","original":"e45507fd","signature":"diagnoseContextualFunctionCallGenericAmbiguity(swift::constraints::ConstraintSystem&, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>)::$_4::operator()(std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::diagnoseAmbiguityWithFixes"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  b: Double
  func callAsFunction<c>( () -> c) -> c
}
let d = a(b: 2)
let : String = d {
