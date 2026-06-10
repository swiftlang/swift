// {"kind":"typecheck","signature":"(anonymous namespace)::AttributeChecker::visitImplementsAttr(swift::ImplementsAttr*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"TypeChecker::checkDeclAttributes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a { b }
@_implements(a, b) typealias c =
