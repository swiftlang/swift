// {"kind":"typecheck","original":"b713d815","signature":"(anonymous namespace)::ExprRewriter::visitInterpolatedStringLiteralExpr(swift::InterpolatedStringLiteralExpr*)","signatureAssert":"Assertion failed: (conformance && \"string interpolation type conforms to protocol\"), function operator()","signatureNext":"ExprWalker::walkToExprPost"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
    appendLiteral( String)
    appendInterpolation<b>(b )
  func c<d: ExpressibleByStringInterpolation>(
    x: Int
  ) -> d where d.StringInterpolation == a {
    "\(x)"
