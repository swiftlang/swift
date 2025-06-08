// {"signature":"swift::PackType::getExpandedGenericArgs(llvm::ArrayRef<swift::GenericTypeParamType*>, llvm::ArrayRef<swift::Type>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < each b typealias c<each d> = a<> c < e
