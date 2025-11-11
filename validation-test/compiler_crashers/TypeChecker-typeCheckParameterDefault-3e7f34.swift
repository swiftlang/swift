// {"kind":"typecheck","original":"fec80ed5","signature":"swift::TypeChecker::typeCheckParameterDefault(swift::Expr*&, swift::DeclContext*, swift::Type, bool, bool)","signatureAssert":"Assertion failed: (paramType && (!paramType->hasError() || DC->getASTContext().CompletionCallback)), function typeCheckParameterDefault"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a {
  return switch 0 {
  case    /fallthrough
    fatalError(
