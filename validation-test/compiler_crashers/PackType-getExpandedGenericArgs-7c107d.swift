// {"kind":"typecheck","signature":"swift::PackType::getExpandedGenericArgs(llvm::ArrayRef<swift::GenericTypeParamType*>, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < each b typealias c<each d> = a<> c < e
