// {"kind":"typecheck","original":"96d4a6fd","signature":"(anonymous namespace)::ContextualizationWalker::walkToExprPre(swift::Expr*)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
@expression macro a(
  Int  =
    #a()
) -> Int
#a(
