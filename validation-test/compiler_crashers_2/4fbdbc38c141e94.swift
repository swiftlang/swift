// {"signature":"swift::TypeChecker::typeCheckParameterDefault(swift::Expr*&, swift::DeclContext*, swift::Type, bool, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
init<a>(b: a == {
