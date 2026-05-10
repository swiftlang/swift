// {"kind":"typecheck","original":"c9dda487","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  class b {
    subscript(c: String) -> some a {
      get {
        self[c]
      }
      set
    }
  }
}
