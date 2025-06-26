// {"signature":"swift::TypeChecker::checkDeclAttributes(swift::Decl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a { b }
@_implements(a, b) typealias c =
