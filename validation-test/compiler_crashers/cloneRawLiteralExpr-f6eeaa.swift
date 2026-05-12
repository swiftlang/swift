// {"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"2f906db1","signature":"cloneRawLiteralExpr(swift::ASTContext&, swift::LiteralExpr*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"deriveBodyRawRepresentable_raw"}
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 %s
struct a {
  enum b: Character {
    case
  }
  var c = b.rawValue
}
