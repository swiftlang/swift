// {"kind":"typecheck","signature":"swift::rewriting::RewriteContext::getRelativeTermForType(swift::CanType, llvm::ArrayRef<swift::rewriting::Term>)","signatureAssert":"Assertion failed: (result.back().getKind() != Symbol::Kind::Shape), function getRelativeTermForType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// https://github.com/swiftlang/swift/issues/84490
struct a < b > {
  func
    c < each d where (repeat each d , b) == b>()
}
