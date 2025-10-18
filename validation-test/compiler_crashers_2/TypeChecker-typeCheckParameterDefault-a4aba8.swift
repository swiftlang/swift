// {"kind":"typecheck","original":"c03f63d6","signature":"swift::TypeChecker::typeCheckParameterDefault(swift::Expr*&, swift::DeclContext*, swift::Type, bool, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b, each c>(repeat each b -> each c = {
