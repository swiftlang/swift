// {"kind":"typecheck","languageMode":6,"signature":"swift::AnyFunctionType::isSendable() const","signatureAssert":"Assertion failed: (!hasSendableDependentType() && \"Query Sendable dependence first\"), function isSendable"}
// RUN: not %target-swift-frontend -typecheck -swift-version 6 %s
[].reduce = {
}
