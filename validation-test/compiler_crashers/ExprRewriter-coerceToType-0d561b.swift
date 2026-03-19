// {"kind":"typecheck","original":"35c38675","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ExprWalker::rewriteTarget"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder enum a<b {
  static  buildBlock(b ) -> [[b]]
}
protocol c {
  associatedtype b
}
struct d: c
  struct e<f: c {
    init(@a<f.b> body: () -> [[f.b]]
  }
  let : e<d> = e {
    []
