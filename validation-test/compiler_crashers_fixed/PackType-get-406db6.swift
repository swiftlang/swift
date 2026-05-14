// {"kind":"typecheck","original":"867b7a5a","signature":"swift::PackType::get(swift::ASTContext const&, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"PackConformance::getAssociatedConformance"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol c {
  associatedtype d
  associatedtype e: a
}
struct f<g, h, i>: c {
  typealias d = g
  func j<each k: c>(repeat each k, l: (repeat each k.d))
    -> (repeat each k.e.b)
  {
    let m = f<Int, String, Never>(
    let n = f<Int, String, AnyHashable>(
    let _  j(m
    n
    , l: (0 0
