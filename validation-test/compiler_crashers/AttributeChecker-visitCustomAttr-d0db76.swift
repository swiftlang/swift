// {"kind":"typecheck","original":"6e104d66","signature":"(anonymous namespace)::AttributeChecker::visitCustomAttr(swift::CustomAttr*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"TypeChecker::checkDeclAttributes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder struct a {
  func b() {
    { (@a ) in
    } {
    }
  }
}
