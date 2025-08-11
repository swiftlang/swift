// {"kind":"typecheck","signature":"swift::TypeChecker::checkDeclAttributes(swift::Decl*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a { b }
@_implements(a, b) typealias c =
