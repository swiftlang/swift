// {"kind":"typecheck","languageMode":6,"original":"00d714c6","signature":"swift::AnyFunctionType::isSendable() const","signatureAssert":"Assertion failed: (!hasSendableDependentType() && \"Query Sendable dependence first\"), function isSendable"}
// RUN: not %target-swift-frontend -typecheck -swift-version 6 %s
enum a {
  case b(String, String, AnyObject)
  c {
  switch self {
  case let.b(d e, f)let elements [e.map , [ {}
  ].flatMap
  }
