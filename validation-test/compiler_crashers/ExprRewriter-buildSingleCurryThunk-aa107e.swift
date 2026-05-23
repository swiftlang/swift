// {"kind":"typecheck","original":"ef834ab9","signature":"(anonymous namespace)::ExprRewriter::buildSingleCurryThunk(swift::Expr*, swift::Expr*, swift::DeclContext*, swift::FunctionType*, swift::FunctionType*, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"ExprRewriter::buildDoubleCurryThunk"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b {
  c: (repeat each b)
  func d(repeat (each b))
}
let e = a(
  c: (
let _ e.d
