// {"kind":"typecheck","original":"ef834ab9","signature":"swift::PackType::get(swift::ASTContext const&, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"PackConformance::getAssociatedConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol c: a where b: a
  struct d: c
    struct e<each f: c {
      g: (repeat each f)
      func h(repeat (each f).b.b)
    }
    let i = e(
      g: d(
    let _ i.h
