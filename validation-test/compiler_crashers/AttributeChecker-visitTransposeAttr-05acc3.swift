// {"kind":"typecheck","original":"240d55bd","signature":"(anonymous namespace)::AttributeChecker::visitTransposeAttr(swift::TransposeAttr*)","signatureAssert":"Assertion failed: (idx < size()), function operator[]","signatureNext":"TypeChecker::checkDeclAttributes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@transpose(of: a, wrt: 0) func b(c: <#type#>)
