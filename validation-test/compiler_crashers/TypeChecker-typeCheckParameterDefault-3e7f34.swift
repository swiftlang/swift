// {"kind":"typecheck","original":"fec80ed5","signature":"swift::TypeChecker::typeCheckParameterDefault(swift::Expr*&, swift::DeclContext*, swift::Type, bool, bool)","signatureAssert":"Assertion failed: (paramType && (!paramType->hasError() || DC->getASTContext().CompletionCallback)), function typeCheckParameterDefault","signatureNext":"CallerSideDefaultArgExprRequest::evaluate"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a {
  return switch 0 {
  case    /fallthrough
    fatalError(
