// {"kind":"typecheck","languageMode":6,"original":"00437d6c","signature":"swift::AnyFunctionType::isSendable() const","signatureAssert":"Assertion failed: (!hasSendableDependentType() && \"Query Sendable dependence first\"), function isSendable"}
// RUN: not %target-swift-frontend -typecheck -swift-version 6 %s
let a: [Double]
  a.map {
  }.map & {
