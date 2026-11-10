// {"kind":"typecheck","original":"96d4a6fd","signature":"(anonymous namespace)::ContextualizationWalker::walkToExprPre(swift::Expr*)","stackOverflow":true}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@expression macro a(
  Int  =
    #a()
) -> Int
#a(
