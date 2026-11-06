// {"kind":"typecheck","signature":"swift::PackType::getExpandedGenericArgs(llvm::ArrayRef<swift::GenericTypeParamType*>, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"TypeAliasType::getExpandedGenericArgs"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b
  typealias c<each d> = a<>
    c<(
