// {"kind":"typecheck","original":"e62b3704","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType","signatureNext":"FindCapturedVars::checkType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  protocol c: a where d == b {
    func e<each f: c>(repeat: repeat each f, g: <#type#>)
    where
      repeat (each f)
        .b
        == each f
  }
}
