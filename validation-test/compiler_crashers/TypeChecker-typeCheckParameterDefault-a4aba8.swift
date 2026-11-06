// {"kind":"typecheck","original":"c03f63d6","signature":"swift::TypeChecker::typeCheckParameterDefault(swift::Expr*&, swift::DeclContext*, swift::Type, bool, bool)","signatureNext":"DefaultArgumentExprRequest::evaluate"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b, each c>(repeat each b -> each c = {
