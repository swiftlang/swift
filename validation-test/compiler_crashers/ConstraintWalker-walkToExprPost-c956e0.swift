// {"kind":"typecheck","original":"1562769e","signature":"(anonymous namespace)::ConstraintWalker::walkToExprPost(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum c
  func d()  e {
    if let
      f = c
    {
      switch f {
      case let a(b) where b
