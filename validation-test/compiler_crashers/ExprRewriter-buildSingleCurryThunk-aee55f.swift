// {"kind":"typecheck","original":"9eb2d438","signature":"(anonymous namespace)::ExprRewriter::buildSingleCurryThunk(swift::Expr*, swift::Expr*, swift::DeclContext*, swift::FunctionType*, swift::FunctionType*, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"ExprRewriter::buildStaticCurryThunk"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  static func c<each d>(repeat each d) -> b
  struct e<g {
    f: () -> g  init < d
    : a where d.b == g {
    f = d.c
