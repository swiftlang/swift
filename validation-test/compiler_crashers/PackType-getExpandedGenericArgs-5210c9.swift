// {"kind":"typecheck","original":"04466b1b","signature":"swift::PackType::getExpandedGenericArgs(llvm::ArrayRef<swift::GenericTypeParamType*>, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"PackReferenceCollector::walkToTypePre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b {
  typealias c<each d> = (
}
func e<each f {
  repeat a<each f>.c
