// {"kind":"typecheck","original":"04466b1b","signature":"swift::PackType::getExpandedGenericArgs(llvm::ArrayRef<swift::GenericTypeParamType*>, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"PackReferenceCollector::walkToTypePre"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b {
  typealias c<each d> = (
}
func e<each f {
  repeat a<each f>.c
